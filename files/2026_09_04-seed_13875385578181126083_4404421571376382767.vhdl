-- Seed: 13875385578181126083,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity xcnm is
  port (vjv : in real; ihjpbydagw : buffer character; nx : buffer std_logic_vector(3 to 1); rqfmfljem : inout boolean_vector(0 downto 4));
end xcnm;

architecture qohgfxghm of xcnm is
  
begin
  -- Single-driven assignments
  rqfmfljem <= (others => TRUE);
  ihjpbydagw <= ihjpbydagw;
  
  -- Multi-driven assignments
  nx <= "";
  nx <= "";
  nx <= nx;
  nx <= "";
end qohgfxghm;

entity cwlxyl is
  port (qczjyy : out integer; vnzmwzv : in integer);
end cwlxyl;

library ieee;
use ieee.std_logic_1164.all;

architecture aorpltol of cwlxyl is
  signal cwgjmzrh : boolean_vector(0 downto 4);
  signal zvt : character;
  signal ls : boolean_vector(0 downto 4);
  signal volbdd : std_logic_vector(3 to 1);
  signal wfghb : character;
  signal dusklxk : real;
begin
  l : entity work.xcnm
    port map (vjv => dusklxk, ihjpbydagw => wfghb, nx => volbdd, rqfmfljem => ls);
  zykukad : entity work.xcnm
    port map (vjv => dusklxk, ihjpbydagw => zvt, nx => volbdd, rqfmfljem => cwgjmzrh);
  
  -- Single-driven assignments
  qczjyy <= vnzmwzv;
  dusklxk <= dusklxk;
end aorpltol;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (kgkgl : linkage std_logic_vector(3 downto 3); hfpehdizyn : buffer std_logic_vector(2 to 2));
end n;

architecture lkpcqvoo of n is
  signal tvyhzqwth : integer;
  signal cfyk : integer;
  signal qpagw : integer;
  signal nzckjrqjm : integer;
begin
  yiob : entity work.cwlxyl
    port map (qczjyy => nzckjrqjm, vnzmwzv => qpagw);
  psnw : entity work.cwlxyl
    port map (qczjyy => qpagw, vnzmwzv => cfyk);
  zols : entity work.cwlxyl
    port map (qczjyy => cfyk, vnzmwzv => nzckjrqjm);
  gxvufocd : entity work.cwlxyl
    port map (qczjyy => tvyhzqwth, vnzmwzv => qpagw);
end lkpcqvoo;



-- Seed after: 17199110106168843940,4404421571376382767
