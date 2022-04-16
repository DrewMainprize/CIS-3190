--CIS*3190 - Assignment 2: Ada Image Processing
--University of Guelph
--For, Professor Michael Wirth
--
--
--Drew Mainprize
--1045298
with imgrecord; use imgrecord;


package imageprocess is

    type histArr is array(1..256) of float;
    type finalArr is array(1..256) of integer;


    procedure imageINV(myImg : in out img);
    procedure imageLOG(myImg : in out img);
    procedure imageSTRETCH(myImg : in out img; iMin : in integer; iMax : in integer);
    function makeHIST(myImg : in img) return finalArr;
    function histEQUAL(myImg : in img; theHist : finalArr) return img;



end imageprocess;