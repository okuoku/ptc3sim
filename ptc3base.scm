;; types::
;;        num : number
;;        num*: number array
;;        str : string
;;      color : color code ... RGB(x,y,z)
;; animtarget : animation target string or number (BGANIM)
;; listtarget : ERR or slot:linenumber or linenumber
;;        var : variable reference (INC,DEC)
;;        ... : vararg (FORMAT$)
;;   funcname : function name for XON XOFF
;;      label : literal label @HOGE
;;        any : any variable
;;       any* : any array variable
;;
;; Vector is for argument group (BGCLIP etc)
;; UNKNOWN
;; SPECIAL


;; SPECIAL
(OPTION SPECIAL)
(COMMON :keyword :token)
(REM :syntax) ;; FIXME: Comment
(DEF :keyword :syntax SPECIAL)
(DIM :keyword :syntax SPECIAL)
(VAR :keyword :syntax SPECIAL)
(OUT :keyword :syntax :token)
(USE :keyword (slot num))
(LIST (target listtarget :optional) :direct)

;; Control flow
(CALL :keyword (name str) ...)
(CALL :keyword (name str) ... OUT ...)
(CALL :keyword (=> any) (name str) ...)
(FOR :keyword :syntax SPECIAL)
(TO :token :syntax) ;; Token
(STEP :token :syntax) ;; Token
(NEXT :keyword :syntax) ;; FIXME: Ignore variable name
(STOP :keyword)
(RETURN :keyword) ;; Return from gosub-subroutine
(RETURN :keyword (val any)) ;; Return from DEF procedure
(END :keyword :syntax) ;; Also used in DEF
(IF :keyword (check num) :syntax) ;; FIXME: IF - GOTO 
(THEN :keyword :syntax :token)
(ELSE :keyword :syntax :token)
(ENDIF :keyword :syntax :token)
(GOSUB :keyword :syntax (label str))
(GOSUB :keyword :syntax (label label))
(GOTO :keyword :syntax (label str))
(GOTO :keyword :syntax (label label))
(ON :keyword SPECIAL) ;; FIXME: ON - GOTO ON - GOSUB
(WHILE :keyword :syntax (val num))
(WEND :keyword :syntax)
(REPEAT :keyword :syntax)
(UNTIL :keyword :syntax (val num))
(BREAK :keyword :syntax) ;; FOR WHILE REPEAT
(CONTINUE :keyword :syntax) ;; FOR WHILE REPEAT

;; Data statement
(READ :keyword ...)
(DATA :keyword :syntax ...)
(RESTORE :keyword (label str))
(RESTORE :keyword (label label))

;; Operators
(AND :operator :keyword)
(NOT :keyword :operator)
(OR :keyword :operator)
(XOR :keyword :operator)
(MOD :keyword :operator)
(DIV :keyword :operator)


;; Math
(PI (=> num))
(ABS num (=> num))
(ACOS num (=> num))
(ASIN num (=> num))
(ATAN num (=> num))
(POW (=> num) (a num) (b num))
(RAD (=> num) (val num))
(ROUND (=> num) (v num))
(SGN (=> num) (v num))
(SIN (=> num) (v num))
(SINH (=> num) (v num))
(SQR (=> num) (v num))
(TAN (=> num) (angle num))
(TANH (=> num) (v num))
(VAL (=> num) (in str))
(CEIL (=> num) num)
(DEG (=> num) (v num))
(COS (=> num) (angle num))
(COSH (=> num) (v num))
(EXP (=> num) (v num))
(FLOOR (=> num) (v num))
(LOG (=> num) (v num) (e num :optional))

;; Random
(RND (=> num) (seed num :optional) (max num))
(RNDF (=> num) (seed num :optional))
(RANDOMIZE (seedid num) (seed num :optional))


;; Error
(CONT :direct)
(BACKTRACE :direct)
(ERRNUM :sysvar)
(ERRLINE :sysvar)
(ERRPRG :sysvar)

;; Accel
(ACCEL OUT (X num) (Y num) (Z num))

;; DEC/INC
(DEC :keyword var (val num :optional))
(INC :keyword var (val num :optional))

;; Environment check
(CHKCALL (=> num) (name str))
(CHKLABEL (=> num) (label str))
(CHKVAR (=> num) (label str))

;; Misc
(CLEAR :direct)
(KEY (id num) (val str))

;; Drawing
(GBOX (sx num) (sy num) (ex num) (ey num) (c color :optional))
(GCIRCLE (x num) (y num) (r num) (c color :optional))
(GCLIP (mode num) #((sx num) (sy num) (ex num) (ey num) :optional))
(GCLS (c color :optional))
(GCOLOR (c color))
(GCOPY (from num :optional) (sx num) (sy num) (sx num) (sy num) (dx num)
       (dy num) (mode num))
(GFILL (sx num) (sy num) (ex num) (ey num) (c color :optional))
(GLINE (sx num) (sy num) (ex num) (ey num) (c color :optional))
(GLOAD #((x num) (y num) (w num) (h num) :optional)
       (dat num*)
       (colorconv num)
       (mode num))
(GPAGE (disp num) (opr num))
(GPAINT (x num) (y num)
        #((fc color) (bc color :optional) :optional))
(GPRIO (z num))
(GPSET (x num) (y num) (c color :optional))
(GSAVE (from num :optional) #((x num) (y num) (w num) (h num) :optional)
       (dest num*) (conv num))
(GSPOIT (=> num) (x num) (y num))


;; Gyro
(GYROA OUT (p num) (r num) (y num))
(GYROV OUT (p num) (r num) (y num))
(GYROSYNC)

;; Mic
(MICDATA (=> num) (pos num))
(MICPOS :sysvar)
(MICSAVE #((pos num :optional) (count num) :optional) var)
(MICSIZE :sysvar)
(MICSTART (rate num) (bit num) (sec num))
(MICSTOP)

;; Dialog
(RESULT :sysvar)
(DIALOG (message str))
(DIALOG (=> num) (message str) (type num :optional) (caption str :optional)
        (timeout num :optional))


;; Color
(RGB (=> num) (a num :optional) (r num) (g num) (b num))
(RGBREAD (c color) OUT (r num) (g num) (b num))

;; Program/Project
(EXEC :keyword (filename str))
(EXEC :keyword (slot num))
(RUN (slot num :optional) :direct)
(NEW (slot num :optional) :direct)
(PRGDEL (count num :optional))
(PRGEDIT (slot num) (line num :optional))
(PRGGET$ (=> str))
(PRGINS (code str)) ;; CHR(10) for newline
(PRGNAME$ (=> str) (slot num :optional))
(PRGSET (code str))
(PRGSIZE (=> num) (slot num :optional))
(PRGSLOT :sysvar)
(PROJECT (name str) :direct)

;; Filesystem
(CHKFILE (=> num) (filename str))
(LOAD (filename str) (flag num :optional))
(LOAD (filename str) (flag num :optional) OUT var)
(LOAD (=> str) (filename str) (flag num :optional))
(SAVE (name str))
(SAVE (name str) (dat str))
(SAVE (name str) (dat num*))
(RENAME (old str)  (new str))
(FILES (type str :optional))
(FILES (type str :optional) (out strarray)) ;; FIXME:: OUT ??
(DELETE (filename str))

;; Console
(CSRX :sysvar)
(CSRY :sysvar)
(CSRZ :sysvar)
(CHKCHR (=> num) (x num) (y num))
(COLOR (c num) (bg num :optional))
(ATTR num)
(CLS)
(LOCATE (x num :optional) (y num :optional) (z num :optional))
(SCROLL (x num) (y num))
(TABSTEP :sysvar)
(INKEY$ (=> str))
(FONTDEF (code num) (dat str))
(PRINT :keyword ...)
(INPUT :keyword (guide str :optional) ...)
(LINPUT :keyword ...)

;; Strings
(CHR$ (=> str) (code num))
(ASC str (=> num))
(LEFT$ (=> str) (from str) (count num))
(STR$ (=> str) (x num) (col num :optional))
(RIGHT$ (=> str) (from str) (count num))
(SUBST$ (=> str) (from str) (pos num) (count num :optional) (target str))
(MID$ (=> str) (from str) (start num) (count num))
(INSTR (=> num) (start num :optional) (from str) (target str))
(HEX$ (=> str) (val num) (col num :optional))
(FORMAT$ (=> str) (fmt str) ...)

;; Variable 
(POP (=> any) any*)
(SHIFT (=> any) any*)
(UNSHIFT any* any)
(PUSH any* any) 
(SORT #((fromstart num) (fromcount num) :optional) ...)
(RSORT #((fromstart num) (fromcount num) :optional) ...)
(SWAP :keyword any any)
(MIN (=> num) ...)
(MAX (=> num) ...)
(LEN (=> num) str)
(LEN (=> num) any*)
(COPY (to any*) (offset num :optional) (from any*)
      #((fromoffset num :optional) (fromcount num) :optional))
(COPY (to any*) (offset num :optional) (fromlabel str) (count num :optional))

;; Date/Time
(TIME$ :sysstrvar)
(TMREAD (in str :optional) OUT (h num) (m num) (s num))
(DATE$ :sysstrvar)
(DTREAD (dat str :optional) OUT (y num) (m num) (d num) (dw num :optional))

;; System variables(misc)
(TRUE :keyword :sysvar)
(FALSE :keyword :sysvar)
(VERSION :sysvar)
(FREEMEM :sysvar)

;; XON/XOFF
(XOFF funcname)
(XON funcname)

;; VSYNC
(MAINCNT :sysvar)
(VSYNC (frames num :optional))
(WAIT (frames num :optional))

;; Graphics config
(DISPLAY (id num))
(ACLS)
(BACKCOLOR color)
(BACKCOLOR (=> color))
(XSCREEN (mode num) #((sprcount num) (bgcount num) :optional))
(VISIBLE (console num) (grp num) (bg num) (sprite num))

;; Pad/Touch
(STICK (termid num :optional) OUT (x num) (y num))
(STICKEX (termid num :optional) OUT (x num) (y num))
(BUTTON (=> num) #((funcid num) (termid num :optional)))
(BREPEAT (funcid num) (starttime num) (interval num))
(TOUCH (termid num :optional) OUT (time num) (x num) (y num))

;; Audio
(TALK (doc str))
(TALKCHK (=> num))
(TALKSTOP)
(SYSBEEP :sysvar)
(WAVSET (id num) (a num) (d num) (s num) (r num) (wave str) 
        (rate num :optional))
(WAVSETA (id num) (a num) (d num) (s num) (r num) (wave num*) 
         (rate num :optional) (start num :optional) (end num :optional))
(EFCOFF)
(EFCON)
(EFCSET (id num))
(EFCWET (beep num) (bgm num) (talk num))
(BEEP (effect-number num :optional) (freq num :optional) (vol num :optional)
      (pan num :optional))

;; BGM
(BGMCHK (=> num) (track num :optional))
(BGMCLEAR (song num :optional))
(BGMPLAY (track num :optional) (song num) (vol num :optional))
(BGMPLAY (mml str))
(BGMSET (song num) (mml str))
(BGMSETD (song num) (label str))
(BGMSTOP #((track num) (fadetime num :optional) :optional))
(BGMVAR (track num) (var num) (val num))
(BGMVAR (=> num) (track num) (var num))
(BGMVOL (track num :optional) (vol num))

;; BG
(BGCLIP (layer num) 
        #((sx num) (sy num) (ex num) (ey num) :optional))
(BGCLR (layer num :optional))
(BGCOORD (layer num) (x num) (y num) (mode num :optional)
         OUT
         (outx num) (outy num))
(BGCOPY (layer num) (sx num) (sy num) (ex num) (ey num) (dx num) (dy num))
(BGFILL (layer num) (sx num) (sy num) (ex num) (ey num) (c num))
(BGFILL (layer num) (sx num) (sy num) (ex num) (ey num) (scr num))
(BGGET (=> num) (layer num) (x num) (y num) (p num :optional))
(BGHIDE UNKNOWN)
(BGHOME (layer num) (x num) (y num))
(BGLOAD (layer num) #((x num) (y num) (w num) (h num) :optional)
        (dat num*))

(BGOFS (layer num) (x num) (y num) (z num :optional))
(BGPAGE (page num))
(BGPUT (layer num) (x num) (y num) (c num))
(BGPUT (layer num) (x num) (y num) (scr num))
(BGROT (layer num) (angle num))
(BGSAVE (layer num) #((x num) (y num) (w num) (h num) :optional) (dat num*))
(BGSCALE (layer num) (x num) (y num))
(BGSCREEN (layer num) (w num) (h num))
(BGSHOW UNKNOWN)
(BGVAR (layer num) (id num) (val num))
(BGVAR (layer num) (id num) (=> num))
(BGVAR (layer num) (id num) OUT (val num))

;; BG Animation
(BGCHK (layer num) (=> num))
(BGANIM (layer num) animtarget (animdata num*)
        (loopcount num :optional))  
(BGSTART (layer num :optional))
(BGSTOP (layer num :optional))


;; Sprite
(SPCHR (id num) (template num))
(SPCHR (id num) (u num) (v num) #((w num) (h num) :optional) (attr num))
(SPCLIP #((sx num) (sy num) (ex num) (ey num) :optional))
(SPCLR (id num))
(SPCOLOR (id num) (c color))
(SPCOLOR (id num) OUT (c color))
(SPDEF (id num) (u num) (v num) #((w num) (h num) 
                                  #((bx num) (by num) :optional)
                                  :optional)
       (attr num :optional))
(SPDEF num*)
(SPDEF (label str))
(SPDEF (id num) OUT
       (u num) (v num) #((w num) (h num) 
                                 #((bx num) (by num) :optional)
                                 :optional)
       (attr num :optional))
(SPHIDE (id num))
(SPHOME (id num) (x num) (y num))
(SPOFS (id num) (x num) (y num) (z num))
(SPPAGE (page num))
(SPROT (id num) (ang num))
(SPROT (id num) OUT (ang num))
(SPROT (id num) (=> num))
(SPSCALE (id num) (x num) (y num))
(SPSCALE (id num) OUT (x num) (y num))
(SPSET (id num) (template num))
(SPSET (id num) (u num) (v num) #((w num) (h num) :optional) (attr num))
(SPSHOW (id num))

;; Sprite animation/collision/link states
(SPANIM (id num) animtarget (dat num*) (loop num :optional)) ;; FIXME:
(SPANIM (id num) animtarget (label str) (loop num :optional))
(SPANIM (id num) animtarget ...)
(SPCHK (=> num) (id num))
(SPCOL (id num) #((x num) (y num) (w num) (h num) :optional)
       (scale num :optional) (mask num :optional))
(SPCOLVEC (id num) #((vx num) (vy num) :optional))
(SPVAR (id num) (v num) (val num))
(SPVAR (=> num) (id num) (v num))
(SPVAR (id num) (v num) OUT (val num))
(SPHITINFO OUT (time num))
(SPHITINFO OUT (time num) (x1 num) (y1 num) (x2 num) (y2 num))
(SPHITINFO OUT (time num) (x1 num) (y1 num) (vx1 num) (vy1 num)
           (x2 num) (y2 num) (vx2 num) (vy2 num))
(SPHITRC (=> num)
         ...) ;; FIXME: Unmatched in document??
(SPHITSP (=> num) (id num :optional))
(SPHITSP (=> num) (id1 num) (id2 num))
(SPSTART (id num :optional))
(SPSTOP (id num :optional))
(SPUNLINK (id num))
(SPLINK (id num) (to num))

;; Multiplayer
(MPCOUNT :sysvar)
(MPEND)
(MPGET (=> num) (termid num) (id num))
(MPHOST :sysvar)
(MPLOCAL :sysvar)
(MPNAME$ (termid num))
(MPRECV OUT (from num) (dat str)) ;; FIXME: recv to strvar
(MPSEND (dat str))
(MPSET (id num) (val num))
(MPSTART (maxuser num) (idstr str))
(MPSTAT (termid num))
