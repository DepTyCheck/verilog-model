-- Seed: 1613550027314532764,3400751927341804175

entity egkuhvuxog is
  port (e : out bit_vector(2 downto 2); lyzcp : in time);
end egkuhvuxog;

architecture rzfko of egkuhvuxog is
  
begin
  -- Single-driven assignments
  e <= e;
end rzfko;

entity pf is
  port (hfsjwazw : inout real);
end pf;

architecture qqcmwuxeb of pf is
  signal zsd : time;
  signal rgrlbbr : bit_vector(2 downto 2);
begin
  iql : entity work.egkuhvuxog
    port map (e => rgrlbbr, lyzcp => zsd);
end qqcmwuxeb;

library ieee;
use ieee.std_logic_1164.all;

entity oqjhnq is
  port (ebszbh : inout std_logic; zlktfjbd : buffer real);
end oqjhnq;

architecture en of oqjhnq is
  signal dafcobt : time;
  signal iei : bit_vector(2 downto 2);
  signal yzgu : real;
begin
  wktlpom : entity work.pf
    port map (hfsjwazw => yzgu);
  blhqsifts : entity work.egkuhvuxog
    port map (e => iei, lyzcp => dafcobt);
  
  -- Single-driven assignments
  zlktfjbd <= 16#D09EA.D_4_9_7_6#;
  dafcobt <= dafcobt;
  
  -- Multi-driven assignments
  ebszbh <= ebszbh;
end en;



-- Seed after: 1337077663183203360,3400751927341804175
