-- Seed: 17230292413412189108,1834764876137802293

entity sdbru is
  port (mykffpdtv : in real; zfoy : in time; lwc : linkage bit);
end sdbru;

architecture q of sdbru is
  
begin
  
end q;

entity tse is
  port (ul : linkage boolean_vector(4 downto 2));
end tse;

architecture natzramuq of tse is
  signal svhuhmnnhr : bit;
  signal febxuzpues : time;
  signal plvustluwy : bit;
  signal h : real;
  signal pbpmu : bit;
  signal ohyiwfenta : time;
  signal udirnzc : real;
begin
  sj : entity work.sdbru
    port map (mykffpdtv => udirnzc, zfoy => ohyiwfenta, lwc => pbpmu);
  zvjoaqigi : entity work.sdbru
    port map (mykffpdtv => h, zfoy => ohyiwfenta, lwc => plvustluwy);
  qnjibkvww : entity work.sdbru
    port map (mykffpdtv => udirnzc, zfoy => febxuzpues, lwc => svhuhmnnhr);
  
  -- Single-driven assignments
  udirnzc <= 24.2423;
  febxuzpues <= 4_4_1 ns;
  h <= 220.0_2_3_0_4;
  ohyiwfenta <= 4_3_3_0 fs;
end natzramuq;

library ieee;
use ieee.std_logic_1164.all;

entity evunnlvxum is
  port (svtr : buffer real_vector(1 to 4); cwfgrcyegf : buffer std_logic_vector(1 downto 4));
end evunnlvxum;

architecture eqyqneuxj of evunnlvxum is
  signal hddb : boolean_vector(4 downto 2);
  signal iclza : bit;
  signal wlcfiyryqi : time;
  signal orzco : real;
begin
  dxxgwvue : entity work.sdbru
    port map (mykffpdtv => orzco, zfoy => wlcfiyryqi, lwc => iclza);
  xqrzzmhyvi : entity work.tse
    port map (ul => hddb);
  
  -- Multi-driven assignments
  cwfgrcyegf <= "";
end eqyqneuxj;



-- Seed after: 4143714949599213194,1834764876137802293
