-- Seed: 12778993932819524344,6290177331721581829

use std.reflection.all;

entity ejnanssrr is
  port (ahlh : inout integer_value_mirror; dxpbmmdspt : inout floating_subtype_mirror);
end ejnanssrr;

architecture wgnnwtxs of ejnanssrr is
  
begin
  
end wgnnwtxs;

use std.reflection.all;

entity vcqd is
  port (wzw : inout file_value_mirror; vtwote : inout record_value_mirror; vhuy : in time_vector(0 to 4); sqhvexjw : inout access_subtype_mirror);
end vcqd;

use std.reflection.all;

architecture cyvvc of vcqd is
  shared variable mwvt : floating_subtype_mirror;
  shared variable rcwgytrmh : integer_value_mirror;
  shared variable tlzkao : floating_subtype_mirror;
  shared variable k : integer_value_mirror;
begin
  z : entity work.ejnanssrr
    port map (ahlh => k, dxpbmmdspt => tlzkao);
  bu : entity work.ejnanssrr
    port map (ahlh => rcwgytrmh, dxpbmmdspt => mwvt);
end cyvvc;

library ieee;
use ieee.std_logic_1164.all;

entity zxxhxyr is
  port (dkkdbz : inout std_logic);
end zxxhxyr;

use std.reflection.all;

architecture rzoxatetk of zxxhxyr is
  shared variable rhlhbt : floating_subtype_mirror;
  shared variable ss : integer_value_mirror;
  shared variable ccmnbl : floating_subtype_mirror;
  shared variable hbeumhov : integer_value_mirror;
  shared variable onyf : access_subtype_mirror;
  signal hdun : time_vector(0 to 4);
  shared variable g : record_value_mirror;
  shared variable k : file_value_mirror;
  shared variable qrcpbg : floating_subtype_mirror;
  shared variable fsqjtbrkp : integer_value_mirror;
begin
  bv : entity work.ejnanssrr
    port map (ahlh => fsqjtbrkp, dxpbmmdspt => qrcpbg);
  mlnix : entity work.vcqd
    port map (wzw => k, vtwote => g, vhuy => hdun, sqhvexjw => onyf);
  kdqwv : entity work.ejnanssrr
    port map (ahlh => hbeumhov, dxpbmmdspt => ccmnbl);
  ujgx : entity work.ejnanssrr
    port map (ahlh => ss, dxpbmmdspt => rhlhbt);
  
  -- Single-driven assignments
  hdun <= (8#2.7_5_2_0# ms, 1 fs, 8#2_4_3# fs, 16#7_0_6# fs, 2#0_1_0# us);
  
  -- Multi-driven assignments
  dkkdbz <= '0';
  dkkdbz <= 'L';
end rzoxatetk;

use std.reflection.all;

entity qyaroapdvm is
  port (shd : inout array_value_mirror);
end qyaroapdvm;

use std.reflection.all;

architecture rwnk of qyaroapdvm is
  shared variable xpqfpvzmvi : access_subtype_mirror;
  signal znqsudr : time_vector(0 to 4);
  shared variable mpleicmja : record_value_mirror;
  shared variable tznvw : file_value_mirror;
  shared variable fo : floating_subtype_mirror;
  shared variable zdmxkjdhj : integer_value_mirror;
  shared variable mszq : access_subtype_mirror;
  signal xwmdjf : time_vector(0 to 4);
  shared variable elyppqi : record_value_mirror;
  shared variable hpm : file_value_mirror;
  shared variable dlfnutneh : floating_subtype_mirror;
  shared variable jmwllylmoz : integer_value_mirror;
begin
  efgcvqcg : entity work.ejnanssrr
    port map (ahlh => jmwllylmoz, dxpbmmdspt => dlfnutneh);
  frtkhdl : entity work.vcqd
    port map (wzw => hpm, vtwote => elyppqi, vhuy => xwmdjf, sqhvexjw => mszq);
  fzqzdt : entity work.ejnanssrr
    port map (ahlh => zdmxkjdhj, dxpbmmdspt => fo);
  ijpykzmwsu : entity work.vcqd
    port map (wzw => tznvw, vtwote => mpleicmja, vhuy => znqsudr, sqhvexjw => xpqfpvzmvi);
  
  -- Single-driven assignments
  xwmdjf <= (8#5# fs, 0 sec, 3_1_1_4_4 us, 2_2.0031 ps, 16#0_D# us);
  znqsudr <= xwmdjf;
end rwnk;



-- Seed after: 10321032032282886239,6290177331721581829
