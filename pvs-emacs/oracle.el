(defun complete-oracle-name (msg enabled)
  (let* ((oracles (pvs-send-and-wait  
		   (format "(extra-list-oracle-names %s)" enabled)
		   nil nil 'list))
	 (oracle  (completing-read msg
				   (mapcar 'list oracles) nil t)))
    (if (equal oracle "")
	(error "Must provide a oracle name")
      (list oracle))))

(defpvs disable-oracle prove (oracle)
  "Disable oracle. 

Disable external oracle."
  (interactive (complete-oracle-name "Disable external oracle: " t))
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (pvs-send-and-wait (format "(extra-disable-oracle '%s)" oracle)
		     nil nil 'dont-care))

(defpvs enable-oracle prove (oracle)
  "Enable oracle. 

Enable external oracle."
  (interactive (complete-oracle-name "Enable external oracle: " nil))
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (pvs-send-and-wait (format "(extra-enable-oracle '%s)" oracle)
		     nil nil 'dont-care))

(defpvs disable-all-oracles prove ()
  "Disable all oracles. 

Disable all external oracles."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (pvs-send-and-wait "(extra-disable-all-oracles)"
		     nil nil 'dont-care))

(defpvs list-enabled-oracles prove ()
  "List enabled oracles. 

List enabled oracles."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (let ((oracles (pvs-send-and-wait  
		  "(extra-list-oracle-names t)"
		  nil nil 'list)))
    (pvs-message "Enabled oracles: %s" oracles)))

(defpvs list-disabled-oracles prove ()
  "List disabled oracles. 

List disabled oracles."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (let ((oracles (pvs-send-and-wait  
		  "(extra-list-oracle-names nil)"
		  nil nil 'list)))
    (pvs-message "Disabled oracles: %s" oracles)))

