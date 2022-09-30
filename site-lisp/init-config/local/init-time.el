;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(setq display-time-day-and-date t)             ;; 打开日期显示
(setq display-time-format "%Y/%m/%d %H:%M")    ;; 设定时间显示格式
(setq display-time-24hr-format t)              ;; 打开24小时显示模式
(setq display-time-default-load-average nil)   ;; 不显示启动中的系统负荷
(display-time-mode 1)                          ;; 打开时间显示
(display-time)                                 ;; 显示时间

(provide 'init-time)

;;; init-time.el ends here
