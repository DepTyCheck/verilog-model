-- Seed: 1106339714197958071,4404421571376382767

entity svo is
  port (d : buffer integer);
end svo;

architecture p of svo is
  
begin
  -- Single-driven assignments
  d <= d;
end p;

entity yplk is
  port (tmxpfn : out integer; rrkerv : buffer time; hkhf : buffer bit);
end yplk;

architecture wcbdeobt of yplk is
  
begin
  -- Single-driven assignments
  hkhf <= '0';
  tmxpfn <= tmxpfn;
  rrkerv <= 16#A0FD1.26690# us;
end wcbdeobt;

library ieee;
use ieee.std_logic_1164.all;

entity wj is
  port (feuhdl : out std_logic_vector(0 downto 1); npmjvr : in integer; f : inout std_logic; nmhafolxbo : in real);
end wj;

architecture arsupxp of wj is
  signal g : bit;
  signal azhkv : time;
  signal jzsxzajcu : integer;
  signal xvwoamb : integer;
  signal fjonvsetbo : integer;
begin
  vuemvgf : entity work.svo
    port map (d => fjonvsetbo);
  v : entity work.svo
    port map (d => xvwoamb);
  uawbxop : entity work.yplk
    port map (tmxpfn => jzsxzajcu, rrkerv => azhkv, hkhf => g);
  
  -- Multi-driven assignments
  f <= f;
  f <= f;
end arsupxp;



-- Seed after: 13190584993476535326,4404421571376382767
