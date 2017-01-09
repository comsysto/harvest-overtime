npm run build;
git checkout gh-pages;
git rm main.*;
cp -R build/ .;
git add main.*;
git add index.html;
git commit -m "Publish update";
git push origin gh-pages;
git checkout -;
