-- Seed: 9558986911525815414,16461708287571398341

entity kmxv is
  port (djcr : inout integer_vector(1 to 4); crsmpenkmr : out real; xb : linkage integer_vector(2 to 4));
end kmxv;

architecture nhzppexmz of kmxv is
  
begin
  -- Single-driven assignments
  crsmpenkmr <= crsmpenkmr;
  djcr <= djcr;
end nhzppexmz;

entity clor is
  port (cpbfaaw : buffer bit);
end clor;

architecture o of clor is
  signal bzvvcuf : integer_vector(2 to 4);
  signal dhgh : real;
  signal kdcygf : integer_vector(1 to 4);
begin
  tgygfmw : entity work.kmxv
    port map (djcr => kdcygf, crsmpenkmr => dhgh, xb => bzvvcuf);
  
  -- Single-driven assignments
  cpbfaaw <= '0';
end o;

library ieee;
use ieee.std_logic_1164.all;

entity kcvbd is
  port (xrpexql : inout std_logic);
end kcvbd;

architecture uzpjong of kcvbd is
  signal f : bit;
  signal dfr : integer_vector(2 to 4);
  signal s : real;
  signal qtfgwm : integer_vector(1 to 4);
  signal osfabjd : integer_vector(2 to 4);
  signal hrinrauho : real;
  signal upakbulio : integer_vector(1 to 4);
  signal af : bit;
begin
  uklnhxnoa : entity work.clor
    port map (cpbfaaw => af);
  hhburbzxf : entity work.kmxv
    port map (djcr => upakbulio, crsmpenkmr => hrinrauho, xb => osfabjd);
  twqaehztup : entity work.kmxv
    port map (djcr => qtfgwm, crsmpenkmr => s, xb => dfr);
  emo : entity work.clor
    port map (cpbfaaw => f);
  
  -- Multi-driven assignments
  xrpexql <= 'X';
end uzpjong;



-- Seed after: 7267969282494179662,16461708287571398341
