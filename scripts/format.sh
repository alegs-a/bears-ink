# https://astyle.sourceforge.net/astyle.html#_indent-modifiers
printf "
Autoformatting...
"

astyle \
    --ascii \
    --mode=c \
    --style=1tbs \
    --indent=spaces=4 \
    --add-braces \
    --add-one-line-braces \
    --indent-continuation=1 \
    --max-continuation-indent=80 \
    --min-conditional-indent=0 \
    --indent-after-parens \
    --indent-switches \
    --break-blocks \
    --pad-oper \
    --pad-comma \
    --pad-header \
    --suffix=none \
    --align-pointer=name \
    --align-reference=name \
    --break-closing-braces \
    --break-one-line-headers \
    --add-braces \
    --attach-return-type \
    --max-code-length=80 \
    dracula/src/*
