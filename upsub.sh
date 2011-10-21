for file in elisp/*
do
    cd $file && git pull origin master && cd -;
done
