#Requires AutoHotkey v2.0
SetTitleMatchMode 2  ; Enable partial window title matching

; Set a timer to run the handler every 500 milliseconds
SetTimer HandleOverflowDialog, 500

HandleOverflowDialog() {
    ; If a window with "Program Error" in the title exists
    if WinExist("Program Error") {
        WinActivate  ; Bring it to the foreground
        Sleep 100
        Send "i"     ; Press "i" to Ignore
    }
}
