-- Seed: 15811263222224839621,7726014785203345639

use std.reflection.all;

entity lpc is
  port (tzgn : inout physical_value_mirror; v : inout physical_value_mirror; ukb : out time);
end lpc;

architecture sbykcw of lpc is
  
begin
  -- Single-driven assignments
  ukb <= ukb;
end sbykcw;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity p is
  port (xkcqxwnw : inout access_subtype_mirror; slnpzx : inout std_logic; fkxfyusbq : inout file_value_mirror; zug : inout protected_value_mirror);
end p;

use std.reflection.all;

architecture lxo of p is
  signal xqfuzcv : time;
  shared variable hvlb : physical_value_mirror;
  shared variable ffwcxuxnho : physical_value_mirror;
  signal oyazxjifuz : time;
  shared variable zwcu : physical_value_mirror;
  shared variable tifzvwmuaw : physical_value_mirror;
begin
  jvuebqhgsk : entity work.lpc
    port map (tzgn => tifzvwmuaw, v => zwcu, ukb => oyazxjifuz);
  wu : entity work.lpc
    port map (tzgn => ffwcxuxnho, v => hvlb, ukb => xqfuzcv);
  
  -- Multi-driven assignments
  slnpzx <= slnpzx;
  slnpzx <= 'H';
  slnpzx <= slnpzx;
  slnpzx <= slnpzx;
end lxo;

use std.reflection.all;

entity mcz is
  port (fj : inout file_subtype_mirror; qbu : inout floating_subtype_mirror; ymp : inout record_subtype_mirror);
end mcz;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture q of mcz is
  signal nop : time;
  shared variable hegpmgq : physical_value_mirror;
  shared variable ueklr : physical_value_mirror;
  shared variable wenowsjtez : protected_value_mirror;
  shared variable gohvftz : file_value_mirror;
  signal hyy : std_logic;
  shared variable gsidtuyy : access_subtype_mirror;
begin
  tipf : entity work.p
    port map (xkcqxwnw => gsidtuyy, slnpzx => hyy, fkxfyusbq => gohvftz, zug => wenowsjtez);
  erht : entity work.lpc
    port map (tzgn => ueklr, v => hegpmgq, ukb => nop);
  
  -- Multi-driven assignments
  hyy <= '0';
  hyy <= hyy;
  hyy <= '0';
end q;

use std.reflection.all;

entity phghyine is
  port (ydck : inout enumeration_subtype_mirror; kz : inout subtype_mirror);
end phghyine;

use std.reflection.all;

architecture vfnvts of phghyine is
  signal svjzir : time;
  shared variable pllximq : physical_value_mirror;
  shared variable ajktvj : physical_value_mirror;
  shared variable ktjfpapqb : record_subtype_mirror;
  shared variable gy : floating_subtype_mirror;
  shared variable ocjhw : file_subtype_mirror;
  shared variable uemidnj : record_subtype_mirror;
  shared variable oboxluebp : floating_subtype_mirror;
  shared variable ig : file_subtype_mirror;
  signal kukojww : time;
  shared variable qbtkr : physical_value_mirror;
  shared variable hyujhxgb : physical_value_mirror;
begin
  zjeflsi : entity work.lpc
    port map (tzgn => hyujhxgb, v => qbtkr, ukb => kukojww);
  vkmikfhvcm : entity work.mcz
    port map (fj => ig, qbu => oboxluebp, ymp => uemidnj);
  q : entity work.mcz
    port map (fj => ocjhw, qbu => gy, ymp => ktjfpapqb);
  cmlyhn : entity work.lpc
    port map (tzgn => ajktvj, v => pllximq, ukb => svjzir);
end vfnvts;



-- Seed after: 4409723704233963593,7726014785203345639
