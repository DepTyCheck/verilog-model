-- Seed: 13676542774448537819,6290177331721581829

use std.reflection.all;

entity mbja is
  port (unpmz : inout file_value_mirror; mhhxkwjd : inout file_value_mirror; v : inout floating_value_mirror; dzwcpc : out time);
end mbja;

architecture adl of mbja is
  
begin
  -- Single-driven assignments
  dzwcpc <= dzwcpc;
end adl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity gvoob is
  port (bpksr : inout subtype_mirror; gotkjt : linkage std_logic; iyta : inout physical_subtype_mirror);
end gvoob;

use std.reflection.all;

architecture ktgwbp of gvoob is
  signal bdtjmykjgx : time;
  shared variable tl : floating_value_mirror;
  shared variable kmbhdinnxl : file_value_mirror;
  shared variable kouv : file_value_mirror;
  signal exguf : time;
  shared variable lrcri : floating_value_mirror;
  shared variable ktjp : file_value_mirror;
  shared variable fqiwzhzi : file_value_mirror;
  signal pumungby : time;
  shared variable zai : floating_value_mirror;
  shared variable yxb : file_value_mirror;
  shared variable zwga : file_value_mirror;
begin
  dyozxpb : entity work.mbja
    port map (unpmz => zwga, mhhxkwjd => yxb, v => zai, dzwcpc => pumungby);
  ibrex : entity work.mbja
    port map (unpmz => fqiwzhzi, mhhxkwjd => ktjp, v => lrcri, dzwcpc => exguf);
  ssqkywjvu : entity work.mbja
    port map (unpmz => kouv, mhhxkwjd => kmbhdinnxl, v => tl, dzwcpc => bdtjmykjgx);
end ktgwbp;

use std.reflection.all;

entity aezgbxj is
  port (vn : out time; tanlhx : inout record_subtype_mirror; mpel : inout floating_subtype_mirror);
end aezgbxj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture dumo of aezgbxj is
  shared variable getbe : physical_subtype_mirror;
  shared variable n : subtype_mirror;
  shared variable mpxcega : physical_subtype_mirror;
  signal bnqlktm : std_logic;
  shared variable xurcqfgoer : subtype_mirror;
  signal rqyzg : time;
  shared variable b : floating_value_mirror;
  shared variable tvalz : file_value_mirror;
  shared variable qiehwo : file_value_mirror;
  signal dznxuae : time;
  shared variable tuoxc : floating_value_mirror;
  shared variable rnavmb : file_value_mirror;
  shared variable x : file_value_mirror;
begin
  trozwab : entity work.mbja
    port map (unpmz => x, mhhxkwjd => rnavmb, v => tuoxc, dzwcpc => dznxuae);
  jsxvqfgy : entity work.mbja
    port map (unpmz => qiehwo, mhhxkwjd => tvalz, v => b, dzwcpc => rqyzg);
  jib : entity work.gvoob
    port map (bpksr => xurcqfgoer, gotkjt => bnqlktm, iyta => mpxcega);
  l : entity work.gvoob
    port map (bpksr => n, gotkjt => bnqlktm, iyta => getbe);
  
  -- Single-driven assignments
  vn <= dznxuae;
  
  -- Multi-driven assignments
  bnqlktm <= bnqlktm;
  bnqlktm <= 'X';
end dumo;



-- Seed after: 4923228089844934756,6290177331721581829
