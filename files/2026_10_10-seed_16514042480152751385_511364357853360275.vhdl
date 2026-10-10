-- Seed: 16514042480152751385,511364357853360275

entity xmckovdpcl is
  port (hdi : linkage bit_vector(0 downto 2); kacdbtyr : in time; ryet : inout time_vector(2 downto 2));
end xmckovdpcl;

architecture hulsjrsogl of xmckovdpcl is
  
begin
  -- Single-driven assignments
  ryet <= (others => 2#1.0_0_1# ps);
end hulsjrsogl;

library ieee;
use ieee.std_logic_1164.all;

entity mclvocr is
  port (bouhvmqxs : in std_logic_vector(4 downto 1); nfpcqbe : inout real; yjrsbhcx : inout time; lmxpojaps : linkage bit_vector(1 to 3));
end mclvocr;

architecture cf of mclvocr is
  signal pxicoe : time_vector(2 downto 2);
  signal m : bit_vector(0 downto 2);
  signal gyaehpg : time_vector(2 downto 2);
  signal mmyfhl : time;
  signal hbhkczt : bit_vector(0 downto 2);
begin
  gkkvfosmcq : entity work.xmckovdpcl
    port map (hdi => hbhkczt, kacdbtyr => mmyfhl, ryet => gyaehpg);
  fsadpe : entity work.xmckovdpcl
    port map (hdi => m, kacdbtyr => mmyfhl, ryet => pxicoe);
  
  -- Single-driven assignments
  yjrsbhcx <= yjrsbhcx;
  mmyfhl <= 8#3_5_6_1_2.31473# ns;
end cf;

entity nxjkrdl is
  port (f : out real_vector(4 downto 0));
end nxjkrdl;

library ieee;
use ieee.std_logic_1164.all;

architecture huislh of nxjkrdl is
  signal s : bit_vector(1 to 3);
  signal dkwrmqfvxb : real;
  signal yvza : std_logic_vector(4 downto 1);
  signal qdlvc : time_vector(2 downto 2);
  signal qnajqqk : time;
  signal smsmup : bit_vector(0 downto 2);
begin
  ycjl : entity work.xmckovdpcl
    port map (hdi => smsmup, kacdbtyr => qnajqqk, ryet => qdlvc);
  e : entity work.mclvocr
    port map (bouhvmqxs => yvza, nfpcqbe => dkwrmqfvxb, yjrsbhcx => qnajqqk, lmxpojaps => s);
  
  -- Single-driven assignments
  f <= f;
  
  -- Multi-driven assignments
  yvza <= ('L', 'Z', 'U', 'W');
  yvza <= yvza;
  yvza <= yvza;
end huislh;

library ieee;
use ieee.std_logic_1164.all;

entity mnxvjk is
  port (c : in time_vector(3 to 0); xhfcxtd : in std_logic);
end mnxvjk;

architecture zcyst of mnxvjk is
  signal navtg : time_vector(2 downto 2);
  signal quxv : time;
  signal dqxv : bit_vector(0 downto 2);
  signal unak : real_vector(4 downto 0);
begin
  oubsp : entity work.nxjkrdl
    port map (f => unak);
  i : entity work.xmckovdpcl
    port map (hdi => dqxv, kacdbtyr => quxv, ryet => navtg);
  
  -- Single-driven assignments
  quxv <= quxv;
end zcyst;



-- Seed after: 4716348023681631623,511364357853360275
