-- Seed: 16068447599020537733,11387579217500963635



entity mbecjg is
  port (pxlfah : buffer time; n : linkage character; x : buffer real; fominugdd : linkage time);
end mbecjg;



architecture jvmgxhro of mbecjg is
  
begin
  
end jvmgxhro;



entity ubltrkveak is
  port (y : linkage integer_vector(1 downto 0));
end ubltrkveak;



architecture jsrmax of ubltrkveak is
  signal igrqtkzw : real;
  signal kkzqatq : time;
  signal zaeamifo : time;
  signal nslacilfk : real;
  signal sfemgpu : character;
  signal vpj : time;
begin
  lh : entity work.mbecjg
    port map (pxlfah => vpj, n => sfemgpu, x => nslacilfk, fominugdd => zaeamifo);
  tbrkcln : entity work.mbecjg
    port map (pxlfah => kkzqatq, n => sfemgpu, x => igrqtkzw, fominugdd => vpj);
end jsrmax;



entity wmjfua is
  port (wtb : inout integer_vector(2 downto 0));
end wmjfua;



architecture os of wmjfua is
  signal dnzqx : real;
  signal urxh : character;
  signal ozc : time;
begin
  tgpueey : entity work.mbecjg
    port map (pxlfah => ozc, n => urxh, x => dnzqx, fominugdd => ozc);
end os;

library ieee;
use ieee.std_logic_1164.all;

entity wjeuygde is
  port (nqbbwkor : inout std_logic_vector(0 to 3); gwso : out std_logic_vector(3 to 3));
end wjeuygde;



architecture ixpfhd of wjeuygde is
  signal ktjgwflct : integer_vector(2 downto 0);
  signal b : integer_vector(2 downto 0);
begin
  ozgvf : entity work.wmjfua
    port map (wtb => b);
  srqln : entity work.wmjfua
    port map (wtb => ktjgwflct);
end ixpfhd;



-- Seed after: 1705034636545087837,11387579217500963635
