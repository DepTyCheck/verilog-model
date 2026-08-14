-- Seed: 10154829699272772938,8437298063418820479

entity dvljdrtujr is
  port (irwszv : linkage real_vector(4 downto 1); ofxvbpr : buffer boolean; o : buffer real; zqljlsp : buffer time);
end dvljdrtujr;

architecture e of dvljdrtujr is
  
begin
  -- Single-driven assignments
  zqljlsp <= zqljlsp;
  ofxvbpr <= ofxvbpr;
  o <= 2#1.0_0_0#;
end e;

entity vagq is
  port (upwqla : in time);
end vagq;

architecture dqtzvgx of vagq is
  signal vrvylvgxm : time;
  signal jwext : real;
  signal mxngqjyjae : boolean;
  signal k : real_vector(4 downto 1);
  signal vskpxhl : time;
  signal ycpkricdjq : real;
  signal gwix : boolean;
  signal zcs : real_vector(4 downto 1);
begin
  udf : entity work.dvljdrtujr
    port map (irwszv => zcs, ofxvbpr => gwix, o => ycpkricdjq, zqljlsp => vskpxhl);
  qga : entity work.dvljdrtujr
    port map (irwszv => k, ofxvbpr => mxngqjyjae, o => jwext, zqljlsp => vrvylvgxm);
end dqtzvgx;

library ieee;
use ieee.std_logic_1164.all;

entity nzse is
  port (dudahib : in time; ofnbl : in std_logic_vector(2 downto 4); svxuh : in time; mzkzwixhi : inout integer);
end nzse;

architecture gppobqwt of nzse is
  signal ndhqrlrqh : time;
begin
  cdgdqqgmx : entity work.vagq
    port map (upwqla => ndhqrlrqh);
  nhzmtfmad : entity work.vagq
    port map (upwqla => svxuh);
end gppobqwt;

entity aouhf is
  port (ear : in bit);
end aouhf;

architecture lrummy of aouhf is
  signal aw : time;
  signal lfzqpxmdtl : time;
  signal opmdrh : real;
  signal gg : boolean;
  signal opx : real_vector(4 downto 1);
  signal khyqyzixr : time;
  signal uev : real;
  signal hnkfs : boolean;
  signal ctckbsy : real_vector(4 downto 1);
begin
  ovtmwvl : entity work.dvljdrtujr
    port map (irwszv => ctckbsy, ofxvbpr => hnkfs, o => uev, zqljlsp => khyqyzixr);
  qc : entity work.dvljdrtujr
    port map (irwszv => opx, ofxvbpr => gg, o => opmdrh, zqljlsp => lfzqpxmdtl);
  qoztcij : entity work.vagq
    port map (upwqla => aw);
  
  -- Single-driven assignments
  aw <= aw;
end lrummy;



-- Seed after: 7781620572152767801,8437298063418820479
