let s:cmdcount = 0

function! timetrail#log(p)
  if ((!exists("b:timetrail_tick") || b:timetrail_tick != b:changedtick) && (a:p != ""))
    execute "silent! timetrail-log " . shellescape(a:p, 1)
    let b:timetrail_tick = b:changedtick

    let s:cmdcount += 1
    if s:cmdcount > 300
      execute "silent! timetrail-compact"
      let s:cmdcount = 0
    endif
  endif
endfunction

function! timetrail#hold()
  let l:stamp = strftime("%s")
  if (!exists("b:timetrail_stamp") || (b:timetrail_stamp + 120 <= l:stamp))
    call timetrail#log(expand("%:p"))
    let b:timetrail_stamp = l:stamp
  endif
endfunction

augroup Timetrail
  autocmd CursorHold * call timetrail#hold()
  autocmd BufWritePost * call timetrail#log(expand("%:p"))
  autocmd BufUnload * call timetrail#log(expand("<afile>:p"))
augroup END
