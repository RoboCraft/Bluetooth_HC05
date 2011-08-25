(defmacro keyword-style (arg)
  `(span :class "keyword" ,arg))

(defmacro operator-style (arg)
  `(span :class "operator" ,arg))

(defmacro const-style (arg)
  `(span :class "constant" ,arg))

(defmacro num-style (arg)
  `(span :class "number" ,arg))

(defmacro type-style (arg)
  `(span :class "type" ,arg))

(defmacro method-style (arg)
  `(span :class "method" ,arg))

(defmacro chinese-warning ()
  `(span :class "chinese-warning" "狗屁"))

(defmacro line (&body body)
  `(progn ,@body (br)))

(defmacro lines (&body body)
  `(progn
    ,@(loop for ln in body collecting `(line ,ln))))

(defmacro desc (&body body)
  `(line (blockquote ,@body)))

(defconstant +indent+ "  ")
(defmacro >> () `(format t +indent+))

(defmacro hlink (address text)
  `(lml-format "<a href=\"~a\">~a</a>" ,address ,text))

(defmacro hlink-from-pair (pair)
  `(hlink (cdr ,pair) (car ,pair)))

(defmacro description-with-link (description link-pair)
 `(html-string
    (lml-format ,description
      (html-string (hlink-from-pair ,link-pair)))))

(defmacro section (name &body body)
  `(h2 (a :name ,name ,@body)))

(defmacro const-description (name description)
 `(p (const-style ,name) (br) ,description))

(defmacro const-descriptions (&rest pairs)
 `(progn
  ,@(loop for pair in pairs collecting
      (if (symbolp pair)
       `(const-description ,(symbol-name pair) ,pair)
       `(const-description ,(first pair) ,(second pair))))))

(defmacro enum-description (name description &body body)
 `(p
    (div :class "box"
      (line (type-style ,name))
      (princ ,description)
      (blockquote
        (const-descriptions ,@body)))))

(defmacro style (&rest args)
  `(with style ,@args))

(defmacro html-string (&body body)
 `(with-output-to-string (lml:*html-output*) ,@body))

(defmacro html-format (format-string &rest args)
 `(lml-format ,format-string
  ,@(loop for arg in args collecting
     `(with-output-to-string (lml:*html-output*) ,arg))))

(defmacro html-format-to-string (format-string &rest args)
 `(with-output-to-string (lml:*html-output*)
    (html-format ,format-string ,@args)))

(defmacro const-description-symbol (symbol)
  `(const-description (,(symbol-name symbol) ,symbol)))

(defmacro makeup-method-args (args indent)
 `(span
  ,@(loop for arg in args
          with n = (length args)
          for i = 0 then (incf i) collecting
     `(span
       ,(if (> n 1) (string-concat "<br />" indent) "")
        (type-style ,(first arg))
        " "
       ,(second arg)
       ,(if (>= (length arg) 3)
          `(span " = " ,(third arg))
          "")
        ;; If i'ts no last argument, then append a comma, a newline and an indent
       ,(if (and (> n 1) (< i (1- n))) "," "")
       ,(if (and (> n 1) (= i (1- n))) "<br />" "")))))

(defmacro method-description (return-type method-name arglist description)
 `(let ((indent-length (length (format nil "~a ~a" ,return-type ,method-name))))
    (p
      (div :class "box"
        (pre
          (type-style ,return-type)
          " "
          (method-style ,method-name)
          "("
          (makeup-method-args ,arglist "  ")
          ");"
          (br))
         ,description))))

(defconstants
  (+xml-prologue-string+ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")
  (+xhtml-prologue-string+ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml10-strict.dtd\">"))

(lml-format "~A~%" +xml-prologue-string+)
(lml-format "~A~%" +xhtml-prologue-string+)
(html :xmlns "http://www.w3.org/1999/xhtml"
  (head
    (title +title+)
    (meta :http-equiv "Content-Type" :content "text/html" :charset "utf-8")

    (style :type "text/css"
      (cssexp:with-css-output (*standard-output*)
        (:.keyword
          :color "#00007f"
          :font-weight "bold")
        (:.operator :color "#301010")
        (:.constant :color "#663300")
        (:.number :color "#007f00")
        (:.type
          :color "#2b71fd"
          );:font-weight "bold")
        (:.method
          :color "#555533"
          );:font-weight "bold")
          ;:font-style "italic")
        (:.box
          :border "2px solid #aaaaaa"
          :padding "0.5em")
        (:.chinese-warning
          :margin "5px 5px"
          :border "2px solid #ff4400"))))

  (body
    (h1 +top-header+)
    (p (html-format +synopsis+ (chinese-warning)))
    (p +module-hints+)

    ;; Table of contents
    (ol (li (hlink "#methods" +methods-header+))
        (li (hlink "#constants" +constants-header+))
        (li (hlink "#types" +types-header+)))

    (section "methods" +methods-header+)
    
    (method-description "void" "begin"
      (("unsigned" "baud_rate" (num-style "38400"))
       ("uint8_t" "reset_pin" (num-style "0xFF"))
       ("uint8_t" "mode_pin" (num-style "0xFF"))
       ("HC05_Mode" "mode" (const-style "HC05_MODE_DATA")))
      +m-begin+)
    
    (method-description "bool" "probe"
      (("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-probe+)
    
    (method-description "void" "hardReset" ()
      (html-format +m-hardReset+ (method-style "begin()")))
    
    (method-description "bool" "softReset"
      (("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-softReset+)
    
    (method-description "bool" "getVersion"
      (("char" "*buffer")
       ("size_t" "buffer_size")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getVersion+ (i "buffer") (i "buffer_size")))
    
    (method-description "bool" "restoreDefaults"
      (("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-restoreDefaults+)
    
    (method-description "bool" "getAddress"
      (("BluetoothAddress" "&address")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getAddress+ (i "address")))
    
    (method-description "bool" "getName"
      (("char" "*buffer")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getName+ (i "buffer") (i (const-style "HC05_NAME_BUFSIZE"))))
    
    (method-description "bool" "setName"
      (("const char" "*name")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setName+)
    
    (method-description "bool" "getRemoteDeviceName"
      (("const BluetoothAddress" "&address")
       ("char" "*buffer")
       ("size_t" "buffer_size")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getRemoteDeviceName+
        (i "address") (i "buffer") (i "buffer_size")))
    
    (method-description "bool" "getRole"
      (("HC05_Role" "&role")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getRole+ (i "role")))
    
    (method-description "bool" "setRole"
      (("HC05_Role" "role")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setRole+)
    
    (method-description "bool" "getDeviceClass"
      (("uint32_t" "&device_class")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getDeviceClass+ (i "device_class")))
    
    (method-description "bool" "setDeviceClass"
      (("uint32_t" "device_class")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setDeviceClass+)
  
    (method-description "bool" "getInquiryAccessCode"
      (("uint32_t" "&iac")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getInquiryAccessCode+ (i "iac")))
  
    (method-description "bool" "setInquiryAccessCode"
      (("uint32_t" "iac")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setInquiryAccessCode+)
  
    (method-description "bool" "getInquiryMode"
      (("HC05_InquiryMode" "&inq_mode")
       ("int16_t" "&max_devices")
       ("uint8_t" "&max_duration")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getInquiryMode+
        (i "inq_mode") (i "max_devices") (i "max_duration") (chinese-warning)))
  
    (method-description "bool" "setInquiryMode"
      (("HC05_InquiryMode" "inq_mode")
       ("int16_t" "max_devices")
       ("uint8_t" "max_duration")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setInquiryMode+)
  
    (method-description "bool" "getPassword"
      (("char" "*buffer")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getPassword+ (i "buffer") (i (const-style "HC05_PASSWORD_BUFSIZE"))))
  
    (method-description "bool" "setPassword"
      (("const char" "*password")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setPassword+)
  
    (method-description "bool" "getSerialMode"
      (("uint32_t" "&speed")
       ("uint8_t" "&stop_bits")
       ("HC05_Parity" "&parity")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getSerialMode+ (i "speed") (i "stop_bits") (i "parity")))
  
    (method-description "bool" "setSerialMode"
      (("uint32_t" "speed")
       ("uint8_t" "stop_bits")
       ("HC05_Parity" "parity")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setSerialMode+)
  
    (method-description "bool" "getConnectionMode"
      (("HC05_Connection" "&connection_mode")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getConnectionMode+ (i "connection_mode")))
  
    (method-description "bool" "setConnectionMode"
      (("HC05_Connection" "connection_mode")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setConnectionMode+)
  
    (method-description "bool" "bind"
      (("const BluetoothAddress" "&address")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-bind+ (i "address")))
  
    (method-description "bool" "getAddressBound"
      (("BluetoothAddress" "&address")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getAddressBound+ (i "address")))
  
    (method-description "bool" "getLeds"
      (("bool" "&led_status")
       ("bool" "&led_connection")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getLeds+ (i "led_status") (i "led_connection")))
  
    (method-description "bool" "setLeds"
      (("bool" "led_status")
       ("bool" "led_connection")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setLeds+)
  
    (method-description "bool" "setPortState"
      (("uint8_t" "port_num")
       ("uint8_t" "port_state")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-setPortState+ (i "port_num") (i "port_state")))
  
    (method-description "bool" "getMultiplePorts"
      (("uint16_t" "&port_states")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getMultiplePorts+ (i "port_states")))
  
    (method-description "bool" "setMultiplePorts"
      (("uint16_t" "port_states")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setMultiplePorts+)
  
    (method-description "bool" "getInquiryAndPagingParams"
      (("uint16_t" "&inquiry_interval")
       ("uint16_t" "&inquiry_duration")
       ("uint16_t" "&paging_interval")
       ("uint16_t" "&paging_duration")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getInquiryAndPagingParams+ (chinese-warning)))
  
    (method-description "bool" "setInquiryAndPagingParams"
      (("uint16_t" "inquiry_interval")
       ("uint16_t" "inquiry_duration")
       ("uint16_t" "paging_interval")
       ("uint16_t" "paging_duration")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-setInquiryAndPagingParams+
        (method-style "getInquiryAndPagingParams()") (chinese-warning)))
  
    (method-description "bool" "getSniffParams"
      (("uint16_t" "&max_time")
       ("uint16_t" "&min_time")
       ("uint16_t" "&retry_interval")
       ("uint16_t" "&sniff_timeout")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getSniffParams+ (chinese-warning)))
  
    (method-description "bool" "setSniffParams"
      (("uint16_t" "max_time")
       ("uint16_t" "min_time")
       ("uint16_t" "retry_interval")
       ("uint16_t" "sniff_timeout")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-setSniffParams+
        (method-style "getSniffParams()") (chinese-warning)))
  
    (method-description "bool" "enterSniffMode"
      (("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-enterSniffMode+ (chinese-warning)))
  
    (method-description "bool" "exitSniffMode"
      (("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-exitSniffMode+ (chinese-warning)))
  
    (method-description "bool" "getSecurityAndEncryption"
      (("HC05_Security" "&security")
       ("HC05_Encryption" "&encryption")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getSecurityAndEncryption+ (i "security") (i "encryption")))
  
    (method-description "bool" "setSecurityAndEncryption"
      (("HC05_Security" "security")
       ("HC05_Encryption" "encryption")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-setSecurityAndEncryption+)
  
    (method-description "bool" "deleteDeviceFromList"
      (("const BluetoothAddress" "&address")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-deleteDeviceFromList+ (i "address")))
  
    (method-description "bool" "deleteAllDevicesFromList"
      (("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-deleteAllDevicesFromList+)
  
    (method-description "bool" "findDeviceInList"
      (("const BluetoothAddress" "&address")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-findDeviceInList+ (i "address")))
  
    (method-description "bool" "countDevicesInList"
      (("uint8_t" "&device_count")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-countDevicesInList+)
  
    (method-description "bool" "getLastAuthenticatedDevice"
      (("BluetoothAddress" "&address")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getLastAuthenticatedDevice+ (i "address")))
  
    (method-description "bool" "getState"
      (("HC05_State" "&state")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-getState+ (i "state")))
  
    (method-description "bool" "initSerialPortProfile"
      (("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-initSerialPortProfile+)
  
    (method-description "bool" "inquire"
      (("InquiryCallback" "callback")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-inquire+ (i "callback") (method-style "inquire()")))
  
    (method-description "bool" "cancelInquiry"
      (("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-cancelInquiry+)
  
    (method-description "bool" "pair"
      (("const BluetoothAddress" "&address")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-pair+ (i "address")))
  
    (method-description "bool" "connect"
      (("const BluetoothAddress" "&address")
       ("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      (html-format +m-connect+ (i "address")))
  
    (method-description "bool" "disconnect"
      (("unsigned long" "timeout" (const-style "HC05_DEFAULT_TIMEOUT")))
      +m-disconnect+)
  
    (method-description "HC05_Result" "getLastError" () ;!const)
      +m-getLastError+)

    (section "constants" +constants-header+)

    (const-descriptions
      HC05_DEFAULT_TIMEOUT
      HC05_INQUIRY_DEFAULT_TIMEOUT
      HC05_PAIRING_DEFAULT_TIMEOUT
      HC05_PASSWORD_MAXLEN
      HC05_PASSWORD_BUFSIZE
      HC05_NAME_MAXLEN
      HC05_NAME_BUFSIZE
      HC05_ADDRESS_MAXLEN
      HC05_ADDRESS_BUFSIZE)

    (section "types" +types-header+)

    (p (div :class "box" (line (type-style "BluetoothAddress")) BluetoothAddress))
    (p (div :class "box" (line (type-style "InquiryCallback")) (html-format InquiryCallback (method-style "inquire()"))))

    (enum-description "HC05_Mode" HC05_Mode
      HC05_MODE_DATA HC05_MODE_COMMAND)

    (enum-description "HC05_Role" HC05_Role
      HC05_ROLE_SLAVE HC05_ROLE_MASTER HC05_ROLE_SLAVE_LOOP)

    (enum-description "HC05_InquiryMode" HC05_InquiryMode
      HC05_INQUIRY_STANDARD
      ("HC05_INQUIRY_RSSI"
        (lml-format HC05_INQUIRY_RSSI
           (html-string (hlink-from-pair +RSSI-wikipedia-link+)))))

    (enum-description "HC05_Parity"
      (description-with-link HC05_Parity +parity-bit-wikipedia-link+)
      HC05_NO_PARITY
      HC05_PARITY_ODD
      HC05_PARITY_EVEN)

    (enum-description "HC05_Connection" HC05_Connection
      ("HC05_CONNECT_BOUND" (span HC05_CONNECT_BOUND (method-style "bind()")))
      HC05_CONNECT_ANY
      ("HC05_CONNECT_SLAVE_LOOP" (span HC05_CONNECT_SLAVE_LOOP
                                       (chinese-warning))))
    (defconstant +security-link+ "http://www.palowireless.com/bluearticles/cc1_security1.asp")

    (enum-description "HC05_Security"
      (html-format-to-string HC05_Security (hlink +security-link+ +security-link-name+))
      ("HC05_SEC_OFF" (span HC05_SEC_OFF (chinese-warning)))
      HC05_SEC_NON_SECURE HC05_SEC_SERVICE HC05_SEC_LINK
      ("HC05_SEC_UNKNOWN" (span HC05_SEC_UNKNOWN (chinese-warning))))

    (enum-description "HC05_Encryption"
      (html-format-to-string HC05_Encryption
        (hlink +security-link+ +security-link-name+)
        (b +packet-encryption-section+))
      HC05_ENC_OFF HC05_ENC_PTP HC05_ENC_PTP_BROADCAST)

    (enum-description "HC05_State" HC05_State
      HC05_INITIALIZED
      ("HC05_READY" (span HC05_READY (chinese-warning)))
      HC05_PAIRABLE HC05_PAIRED HC05_INQUIRING
      HC05_CONNECTING HC05_CONNECTED HC05_DISCONNECTED
      ("HC05_UNKNOWN" (span HC05_UNKNOWN (chinese-warning))))

    (enum-description "HC05_Result" HC05_Result
      HC05_OK
      ("HC05_FAIL" (span HC05_FAIL (chinese-warning)))
      HC05_ERR_TIMEOUT HC05_ERR_ARGUMENT HC05_ERR_DISC_LINK_LOSS
      ("HC05_ERR_DISC_NO_SLC" (chinese-warning))
      ("HC05_ERR_DISC_TIMEOUT" (span HC05_ERR_DISC_TIMEOUT (chinese-warning)))
      ("HC05_ERR_DISC_ERROR" (span HC05_ERR_DISC_ERROR (chinese-warning)))
      HC05_ERR_AT_COMMAND
      ("HC05_ERR_DEFAULT_RESULT" (span HC05_ERR_DEFAULT_RESULT (chinese-warning)))
      HC05_ERR_PSKEY_WRITE HC05_ERR_DEVICE_NAME_TOO_LONG HC05_ERR_NO_DEVICE_NAME
      HC05_ERR_NAP_TOO_LONG HC05_ERR_UAP_TOO_LONG HC05_ERR_LAP_TOO_LONG
      HC05_ERR_NO_PIO_MASK HC05_ERR_NO_PIO_NUMBER HC05_ERR_NO_DEVICE_TYPE
      HC05_ERR_DEVICE_TYPE_TOO_LONG
      HC05_ERR_NO_IAC HC05_ERR_IAC_TOO_LONG HC05_ERR_INVALID_IAC HC05_ERR_NO_PASSWORD
      HC05_ERR_PASSWORD_TOO_LONG HC05_ERR_INVALID_MODULE_ROLE HC05_ERR_INVALID_BAUD_RATE
      HC05_ERR_INVALID_STOP_BITS HC05_ERR_INVALID_PARITY_BITS HC05_ERR_DEVICE_NOT_IN_LIST
      HC05_ERR_SPP_NOT_INITIALIZED HC05_ERR_SPP_REINIT HC05_ERR_INVALID_INQUIRY_MODE
      HC05_ERR_INQUIRY_TIMEOUT_TOO_LONG HC05_ERR_NO_BLUETOOTH_ADDRESS
      HC05_ERR_SECURITY_MODE HC05_ERR_ENCRYPTION_MODE)))
