-- Seed: 10076603418180896326,6290177331721581829

use std.reflection.all;

entity i is
  port (zlawetzs : linkage integer; kwgbp : inout array_subtype_mirror; urciuidtu : inout access_value_mirror; xhsunky : in bit_vector(3 downto 3));
end i;

architecture mxxdxhfic of i is
  
begin
  
end mxxdxhfic;

use std.reflection.all;

entity qcekldxyo is
  port (dcmgzf : in real_vector(2 to 2); kicq : inout integer; hioyaqxct : inout array_subtype_mirror; tpjlel : inout enumeration_value_mirror);
end qcekldxyo;

architecture vrdg of qcekldxyo is
  
begin
  -- Single-driven assignments
  kicq <= 16#770E3#;
end vrdg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity sjf is
  port (vanvevt : buffer integer; qaxjirq : out real; bl : inout subtype_mirror; s : in std_logic_vector(3 to 2));
end sjf;

use std.reflection.all;

architecture bocnyfpw of sjf is
  shared variable glxi : enumeration_value_mirror;
  shared variable qzu : array_subtype_mirror;
  shared variable dgtwrofhun : enumeration_value_mirror;
  shared variable qlivvipp : array_subtype_mirror;
  signal v : integer;
  shared variable xh : enumeration_value_mirror;
  shared variable g : array_subtype_mirror;
  signal yxapr : integer;
  signal wacyytqmsq : real_vector(2 to 2);
  signal nturf : bit_vector(3 downto 3);
  shared variable mlt : access_value_mirror;
  shared variable ylk : array_subtype_mirror;
  signal f : integer;
begin
  xmdfrd : entity work.i
    port map (zlawetzs => f, kwgbp => ylk, urciuidtu => mlt, xhsunky => nturf);
  eahuq : entity work.qcekldxyo
    port map (dcmgzf => wacyytqmsq, kicq => yxapr, hioyaqxct => g, tpjlel => xh);
  afnck : entity work.qcekldxyo
    port map (dcmgzf => wacyytqmsq, kicq => v, hioyaqxct => qlivvipp, tpjlel => dgtwrofhun);
  kyjuvgd : entity work.qcekldxyo
    port map (dcmgzf => wacyytqmsq, kicq => vanvevt, hioyaqxct => qzu, tpjlel => glxi);
  
  -- Single-driven assignments
  nturf <= (others => '1');
end bocnyfpw;



-- Seed after: 1063836506407050638,6290177331721581829
