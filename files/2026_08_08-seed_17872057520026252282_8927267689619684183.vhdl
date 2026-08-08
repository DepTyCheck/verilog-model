-- Seed: 17872057520026252282,8927267689619684183

entity da is
  port (cn : out bit_vector(0 downto 2); xsfflwv : in real_vector(0 downto 0); rsehe : in bit; uxtvnkop : in time);
end da;

architecture mouyfkwiqj of da is
  
begin
  
end mouyfkwiqj;

library ieee;
use ieee.std_logic_1164.all;

entity nzedhwodo is
  port (qbekxqkmls : out std_logic_vector(4 to 1); plaifmfm : inout boolean);
end nzedhwodo;

architecture gausz of nzedhwodo is
  signal wejil : real_vector(0 downto 0);
  signal ndnc : bit_vector(0 downto 2);
  signal uvjgbekpka : bit;
  signal e : real_vector(0 downto 0);
  signal hsowkiqt : bit_vector(0 downto 2);
  signal pecduclax : real_vector(0 downto 0);
  signal o : bit_vector(0 downto 2);
  signal hhpg : time;
  signal rplblcqbe : bit;
  signal smo : real_vector(0 downto 0);
  signal hwwh : bit_vector(0 downto 2);
begin
  qtfio : entity work.da
    port map (cn => hwwh, xsfflwv => smo, rsehe => rplblcqbe, uxtvnkop => hhpg);
  tqhf : entity work.da
    port map (cn => o, xsfflwv => pecduclax, rsehe => rplblcqbe, uxtvnkop => hhpg);
  xuywg : entity work.da
    port map (cn => hsowkiqt, xsfflwv => e, rsehe => uvjgbekpka, uxtvnkop => hhpg);
  dp : entity work.da
    port map (cn => ndnc, xsfflwv => wejil, rsehe => rplblcqbe, uxtvnkop => hhpg);
  
  -- Single-driven assignments
  wejil <= smo;
  hhpg <= 4_0.2_0_3 ps;
  
  -- Multi-driven assignments
  qbekxqkmls <= "";
  qbekxqkmls <= (others => '0');
  qbekxqkmls <= qbekxqkmls;
end gausz;



-- Seed after: 14193279055505642387,8927267689619684183
