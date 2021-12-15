# Guidelines for Submitting Documents

## General Guidelines

This repository has the following structure: 

``` bash
.
├── README.md
├── data
│   ├── draft_data.csv
│   └── yahoo_data.csv
├── ps5.Rmd
├── ps5.pdf
└── questions
    ├── question_1.Rmd
    ├── question_2.Rmd
    └── question_3_optional.Rmd
```

The top level file is the file that you use to compile all of your code. It will "take-in" the code that you've written in each of the `question_*.Rmd` files after you have saved them. Your answers to each question should be placed inside each of the `question_*.Rmd` files. 

- Once you have forked and then cloned this repository, you should immediately be able to knit to a PDF. However there will not be any answers!
- When you begin to answer a question, say, #1, you open `./questions/question_1.Rmd` and write and execute your code in this file. Once you think you've finished answering, you can save this file, and then knit everything completed to this point by returning to the top-level `ps5.Rmd` file. 
- You might notice that in your compiled PDF the question text does not show up. This is on purpose! We assure you that we the instructors and TAs know what the question is asking. By hiding this, we can read your answers without having to also read our questions repeatedly. 

## Submission

- Submission is through Gradescope.
- Your submission needs to be a PDF file that shows the necessasry code to evaluate your solution. This is probably more code than you would show in a report; but it is what you want someone who is collaborating on this to be able to read. 
- You need to associate every page with a question/answer on Gradescope. This makes it so that our TA team can spend their time together talking about experiments and problems with you in office hours, rather than hunting to find your answers.
  
- You do not need to show work for trivial calculations, but showing work is always allowed.
- For answers that involve a narrative response, please feel free to describe the key concept directly and briefly, if you can do so, and do not feel pressure to go on at length.
- For computing answers, please store them in the answer objects we have created. 
- Use inline code evaluation to write your answers.

# Working Collaboratively

We (all!) come from a PhD program where all of the students (now
faculty @ UCB, Harvard, Princeton, UCLA, UofMichigan, UT-Austin)
worked together to make everyone better. We think that the
*collaborative* model  is a fantastic model, and we want to bring it
forward into this class. 

Talk with your friends, work together on
homework, create study groups. We _promise_ that you will learn more
from your friends and classmates than the instructors.

However, collaboration does not mean:

- free-riding on your classmates;
- sending code without conversation, and especially, 
- downloading solutions from past semesters.

If you have a question of whether what you're doing is collaborative
or not, consider whether the person or thing on the other end of the
collaboration could benefit in some form. If the answer is no, or it
is murky, then it is not collaboration. 

In particular, We bet you can find a solution set for each of these
problem sets with enough looking around. Heck, if you want, We'll tell
you where they are.

*But, don't.*

Use this as a chance to learn. 

