


#Usage
The web app comes pre-loaded with an example dataset. Use this example to familiarize yourself with the functions. 

## Uploading data
To upload your own data, click on the "upload data" button. Data files must be comma delimited, with the first row containing a header of the column names. This can be done in Excel by saving the file as .csv. 

## Specify the outcome
The first step is to specify the outcome variable by choosing one from the drop-down list. The outcome variable must be binary, meaning that it can only have 2 levels, typically 0/1. 

## Specify the marker variable(s)
The next step is to specify the marker variable. This can and should be continuous. To create multiple curves on a single plot, check the check box that is labeled "Check to plot multiple curves". Then you can select multiple marker variables from the selection box. 

## Customize
Plot customization options are on the lower right. You can add a title, curve labels, adjust the label positioning, and adjust the cutoff number and font size. 

## Download
Click the download buttons to save your plots to your computer. Downloading the interactive plot will generate html code that can be opened in a web browser. Open this code in a text editor and you can copy it directly into your own web-page. Downloading the print plot will save a pdf version. 

## Help and feedback

We welcome you feedback. If you have difficulties, or suggestions for improvements, send me an email at michael.sachs@nih.gov. 

<hr/>

#What is an ROC curve?

In medicine and other applications, we often use a biomarker or a test to predict an underlying condition, disease state, or future event. For a binary test and a binary disease state, the following table summarizes the possible errors that one can make using the test as a prediction of a disease state measured by some gold standard

<style>
td { border: 1px #aaa solid;
     padding: .2em; 
     }
</style>

<table align="center" style="text-align:center; border:1px; background:transparent; font-size: medium;">
<tr>
<td colspan="2" style="border:none;"></td>
<td colspan="2" style="background:#eeeebb;"><b>Condition<br />
(as determined by "Gold standard")</b></td>
</tr>
<tr>
<td style="border:none;"></td>
<td style="background:#ddddcc;">Total population</td>
<td style="background:#ffffcc;">Condition positive</td>
<td style="background:#ddddaa;">Condition negative</td>
<td style="background:#ddddcc;">Prevalence =<br />
Σ&#160;Condition positive
<div style="border-top:1px solid;">Σ&#160;Total population</div>
</td>
</tr>
<tr>
<td rowspan="2" style="background:#bbeeee;"><b>Test<br />
outcome</b></td>
<td style="background:#ccffff;">Test<br />
outcome<br />
positive</td>
<td style="background:#ccffcc;"><span style="color:#006600;"><b>True positive</b></span></td>
<td style="background:#eedddd;"><span style="color:#cc0000;"><b>False positive</b></span><br />
</td>
<td style="background:#ccffff;"> Positive predictive value (PPV) =<br />
Σ True positive
<div style="border-top:1px solid;">Σ&#160;Test&#160;outcome&#160;positive</div>
</td>
</tr>
<tr>
<td style="background:#aadddd;">Test<br />
outcome<br />
negative</td>
<td style="background:#eedddd;"><span style="color:#cc0000;"><b>False negative</b></span><br />
</td>
<td style="background:#bbeebb;"><span style="color:#006600;"><b>True negative</b></span></td>
<td style="background:#aadddd;">Negative predictive value (NPV) =<br />
Σ True negative
<div style="border-top:1px solid;">Σ&#160;Test&#160;outcome&#160;negative</div>
</td>
</tr>
<tr>
<td style="border:none;"></td>
<td style="border:none;"></td>
<td style="background:#ffffcc;">True positive fraction (TPF, Sensitivity) =<br />
Σ True positive
<div style="border-top:1px solid;">Σ&#160;Condition&#160;positive</div>
</td>
<td style="background:#ddddaa;">False positive fraction (FPF) =<br />
Σ False positive
<div style="border-top:1px solid;">Σ&#160;Condition&#160;negative</div>
</td>
<td style="background:#ddddcc;">Accuracy (ACC) =<br />
Σ&#160;True positive + Σ True negative
<div style="border-top:1px solid;">Σ&#160;Total population</div>
</td>
</tr>
<tr>
<td style="border:none;"></td>
<td style="border:none;"></td>
<td style="background:#ffffcc;">False negative fraction (FNF) =<br />
Σ False negative
<div style="border-top:1px solid;">Σ&#160;Condition&#160;positive</div>
</td>
<td style="background:#ddddaa;">True negative fraction</a> (TNF, Specificity) =<br />
Σ True negative
<div style="border-top:1px solid;">Σ&#160;Condition&#160;negative</div>
</td>
</tr>
<tr>
<td style="border:none;"></td>
</tr>
</table>
This table was adapted from the ROC curve [Wikipedia page](https://en.wikipedia.org/wiki/Receiver_operating_characteristic)


If the test is continuous, say $M$, then a *test positive* is defined as $M > c$. Now we consider measures of accuracy as functions of $c$, i.e.

$$ TPF(c) = Pr\{M > c | D = 1\} $$
$$ FPF(c) = Pr\{M > c | D = 0\} $$

The ROC curve is a plot of $FPF(c)$ versus $TPF(c)$. It's purpose is to allow the viewer to assess the accuracy of the test $M$ for any possible value of the cutoff $c$. This aids in deciding what cutoff to use in practice, comparing different tests for the same thing, and for evaluating the overall accuracy. A key advantage of our approach is that the values of the cutoffs are visible! 
