-- Seed: 16201750810451166247,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (lwm : buffer std_logic; fwtui : inout severity_level; njyruvp : buffer time);
end m;

architecture rsgqm of m is
  
begin
  -- Single-driven assignments
  njyruvp <= 2#11000.0_0_1# ms;
  fwtui <= NOTE;
  
  -- Multi-driven assignments
  lwm <= 'L';
  lwm <= 'X';
  lwm <= 'H';
  lwm <= 'X';
end rsgqm;

entity bsbkfy is
  port (pwlo : out time);
end bsbkfy;

library ieee;
use ieee.std_logic_1164.all;

architecture yohlec of bsbkfy is
  signal rlipgkcds : severity_level;
  signal ada : std_logic;
  signal gfq : time;
  signal iozr : severity_level;
  signal g : std_logic;
  signal oypaorxts : time;
  signal bpiydyme : severity_level;
  signal izcjmi : std_logic;
begin
  y : entity work.m
    port map (lwm => izcjmi, fwtui => bpiydyme, njyruvp => oypaorxts);
  cmgn : entity work.m
    port map (lwm => g, fwtui => iozr, njyruvp => gfq);
  kjxhktxc : entity work.m
    port map (lwm => ada, fwtui => rlipgkcds, njyruvp => pwlo);
  
  -- Multi-driven assignments
  ada <= 'Z';
  g <= 'W';
end yohlec;



-- Seed after: 14891352888151757885,8118127366649987907
