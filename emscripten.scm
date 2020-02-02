;;;============================================================================

;;; File: "emscripten.scm"

;;; Copyright (c) 2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(##c-declare #<<end-of-c-declare

#include <stdio.h>
#include "emscripten.h"

end-of-c-declare
)


;; (##c-declare #<<end-of-c-declare

;; #include <stdio.h>
;; #include "emscripten.h"

;; #define SCHEME_LIBRARY_LINKER ___LNK___app__

;; ___BEGIN_C_LINKAGE
;; extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
;; ___END_C_LINKAGE

;; extern void setup() {

;;   /*
;;    * Setup the Scheme library by calling "___setup" with appropriate
;;    * parameters.  The call to "___setup_params_reset" sets all parameters
;;    * to their default setting.
;;    */

;;   ___setup_params_struct setup_params;

;;   ___setup_params_reset(&setup_params);

;;   setup_params.version = ___VERSION;
;;   setup_params.linker  = SCHEME_LIBRARY_LINKER;

;;   ___ON_THROW(___setup(&setup_params);,);
;; }

;; extern void cleanup() {

;;   /* Cleanup the Scheme library */

;;   ___cleanup();
;; }

;; extern void user_interrupt() {
;;   ___raise_interrupt(___INTR_USER);
;; }

;; extern void heartbeat_interrupt() {
;;   ___raise_interrupt(___INTR_HEARTBEAT);
;; }

;; extern ___SCMOBJ scheme_idle() {
;; #define ___NARGS 0
;;   ___BEGIN_SFUN_SCMOBJ(___GLO__23__23_idle)
;;   ___BEGIN_SFUN_BODY
;;   ___SFUN_CALL_SCMOBJ
;;   ___SFUN_SET_RESULT_SCMOBJ
;;   ___END_SFUN_BODY
;;   ___SFUN_ERROR_SCMOBJ
;;   ___SFUN_SET_RESULT_SCMOBJ
;;   ___END_SFUN_SCMOBJ
;;   return ___result;
;; #undef ___NARGS
;; }

;; extern double idle() {

;;   ___processor_state ___ps = ___PSTATE;
;;   ___SCMOBJ result = ___FIX(0);

;;   ___ON_THROW(result = scheme_idle();,);

;;   if (___FIXNUMP(result))
;;     return -1.0; /* signal program termination */
;;   else
;;     return ___FLONUM_VAL(result);
;; }

;; end-of-c-declare
;; )

;; (define (##idle)

;;   (##declare (not interrupts-enabled))

;;   (##thread-heartbeat!)

;;   (let ((current-processor
;;          (macro-current-processor)))

;;     ;; check if there are runnable threads

;;     (if (##not (##eq? (macro-btq-singleton? current-processor)
;;                       (macro-current-thread)))

;;         ;; there are runnable threads

;;         interval-runnable

;;         ;; there are no runnable threads, so check if there are threads
;;         ;; waiting for a timeout or for a device to become ready

;;         (let* ((next-sleeper
;;                 (macro-toq-leftmost current-processor))
;;                (interval-sleep
;;                 (if (##eq? next-sleeper current-processor)
;;                     +inf.0
;;                     (begin
;;                       ;; There is a sleeping thread, so figure out in
;;                       ;; how much time it needs to wake up.
;;                       (##flmax
;;                        (##fl- (macro-thread-timeout next-sleeper)
;;                               (##current-time-point))
;;                        interval-min-wait))))
;;                (next-condvar
;;                 (macro-btq-deq-next current-processor))
;;                (interval-io
;;                 (if (##eq? next-condvar current-processor)
;;                     interval-no-io-pending ;; I/O is not pending, just relax
;;                     interval-io-pending))) ;; I/O is pending, so come back soon
;;           (##flmin interval-sleep interval-io)))))

(define interval-runnable 0.0)
(set! interval-runnable 0.0)

(define interval-io-pending 0.0)
(set! interval-io-pending 0.010)

(define interval-no-io-pending 0.0)
(set! interval-no-io-pending 1.0)

(define interval-min-wait 0.0)
(set! interval-min-wait 0.0001)

;;;----------------------------------------------------------------------------

;; Remember REPL history from one run of Gambit to the next.

;; (define (##load-repl-history tty)
;;   (##tty-history-set! tty (##local-storage-get "gambit-repl-history")))

;; (define (##save-repl-history tty)
;;   (##local-storage-set "gambit-repl-history" (##tty-history tty)))

;; (define (##setup-repl-history)
;;   (let* ((ct (macro-current-thread))
;;          (channel (##thread-repl-channel-get! ct))
;;          (tty (macro-repl-channel-input-port channel)))
;;     (if (##tty? tty)
;;         (let ((orig-read-expr
;;                (macro-repl-channel-ports-read-expr channel)))
;;           (##load-repl-history tty)
;;           (macro-repl-channel-ports-read-expr-set!
;;            channel
;;            (lambda (channel)
;;              (let ((result (orig-read-expr channel)))
;;                (##save-repl-history tty)
;;                result)))))))

;;;----------------------------------------------------------------------------

(current-directory "~")

(##module-search-order-set! '("~~lib" "~~userlib" "~"))

;;;----------------------------------------------------------------------------

;; Start the main REPL in the primordial thread, and create a service
;; thread which executes the rest of the program (returning back from
;; the C call to ___setup).

;; (define (##main-program-set! thunk)
;;   (##main-set!
;;    (lambda ()
;;      (##continuation-capture
;;       (lambda (cont)

;;         (##setup-repl-history)

;;         (##thread-start!
;;          (make-root-thread
;;           (lambda ()
;;             (let ((orig-denv (macro-thread-denv thread)))
;;               (macro-denv-interrupt-mask-set! orig-denv 8) ;; (expt 2 ___INTR_USER)
;;               ;; Need to take a copy of the dynamic environment because
;;               ;; ##continuation-return-no-winding will change the
;;               ;; thread's dynamic environment
;;               (##continuation-return-no-winding
;;                cont
;;                (lambda (orig-denv)
;;                  ;; Must disable user interrupts in service thread so
;;                  ;; that the idle function won't start a REPL of its own
;;                  ;; on a ctrl-c.  The new interrupt mask will take
;;                  ;; effect at the thread-restore!  operation.
;;                  (macro-thread-save!
;;                   (lambda (thread orig-denv)
;;                     (macro-thread-denv-set! thread orig-denv)
;;                     (macro-thread-restore!
;;                      thread
;;                      (lambda ()
;;                        #f)))
;;                   orig-denv)))))))

;;         (##thread-yield!)

;;         (##continuation-graft cont thunk))))))

;;;============================================================================
