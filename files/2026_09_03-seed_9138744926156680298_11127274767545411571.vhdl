-- Seed: 9138744926156680298,11127274767545411571

entity devd is
  port (twphsepzfw : buffer real; dhsjp : in string(4 downto 1));
end devd;

architecture lrbw of devd is
  
begin
  -- Single-driven assignments
  twphsepzfw <= 31220.20;
end lrbw;

library ieee;
use ieee.std_logic_1164.all;

entity enorkw is
  port (klny : in std_logic; pzsrl : out std_logic; qmxqdlbw : out bit);
end enorkw;

architecture uyvgffzzl of enorkw is
  signal vqyijscv : string(4 downto 1);
  signal epu : real;
  signal kr : string(4 downto 1);
  signal cenyz : real;
  signal xbbucwbui : string(4 downto 1);
  signal ntywgqoxd : real;
  signal pirnmyehc : string(4 downto 1);
  signal kwjms : real;
begin
  bxtseyds : entity work.devd
    port map (twphsepzfw => kwjms, dhsjp => pirnmyehc);
  siecjelkt : entity work.devd
    port map (twphsepzfw => ntywgqoxd, dhsjp => xbbucwbui);
  vxb : entity work.devd
    port map (twphsepzfw => cenyz, dhsjp => kr);
  wvlrt : entity work.devd
    port map (twphsepzfw => epu, dhsjp => vqyijscv);
  
  -- Single-driven assignments
  qmxqdlbw <= qmxqdlbw;
  pirnmyehc <= pirnmyehc;
  
  -- Multi-driven assignments
  pzsrl <= 'Z';
end uyvgffzzl;

library ieee;
use ieee.std_logic_1164.all;

entity crfu is
  port (dqhk : linkage real; trszsiah : inout integer; nhiq : inout std_logic; kzpzxdzmim : inout time);
end crfu;

architecture u of crfu is
  signal bt : string(4 downto 1);
  signal nq : real;
  signal nham : string(4 downto 1);
  signal nbxgds : real;
  signal ql : bit;
begin
  wtl : entity work.enorkw
    port map (klny => nhiq, pzsrl => nhiq, qmxqdlbw => ql);
  pipzurgm : entity work.devd
    port map (twphsepzfw => nbxgds, dhsjp => nham);
  b : entity work.devd
    port map (twphsepzfw => nq, dhsjp => bt);
  
  -- Multi-driven assignments
  nhiq <= nhiq;
  nhiq <= 'Z';
end u;



-- Seed after: 9318638925144437291,11127274767545411571
