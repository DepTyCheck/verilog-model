-- Seed: 11701228341787848520,5472058987609252853

entity ftibladggx is
  port (wlmxcalee : in real; mzbsx : in character);
end ftibladggx;

architecture atg of ftibladggx is
  
begin
  
end atg;

entity qt is
  port (uoxjr : inout boolean; wzkq : in boolean_vector(1 to 0));
end qt;

architecture acvc of qt is
  signal ojvysds : character;
  signal zmuhsb : real;
  signal rxqbyrwmw : character;
  signal kvhfjibgh : real;
begin
  zvzvqkw : entity work.ftibladggx
    port map (wlmxcalee => kvhfjibgh, mzbsx => rxqbyrwmw);
  hxvpslyxe : entity work.ftibladggx
    port map (wlmxcalee => zmuhsb, mzbsx => rxqbyrwmw);
  nkdezsb : entity work.ftibladggx
    port map (wlmxcalee => kvhfjibgh, mzbsx => ojvysds);
  
  -- Single-driven assignments
  ojvysds <= 'q';
  kvhfjibgh <= 8#70131.2_3#;
  uoxjr <= TRUE;
end acvc;

library ieee;
use ieee.std_logic_1164.all;

entity hmnhn is
  port (ezzfny : out std_logic_vector(4 to 2); xpho : inout std_logic);
end hmnhn;

architecture z of hmnhn is
  signal xbxnou : character;
  signal et : real;
  signal prx : character;
  signal tqqfzwuvr : real;
  signal tjyqwwjr : boolean_vector(1 to 0);
  signal twqycc : boolean;
begin
  pgqk : entity work.qt
    port map (uoxjr => twqycc, wzkq => tjyqwwjr);
  bjihmp : entity work.ftibladggx
    port map (wlmxcalee => tqqfzwuvr, mzbsx => prx);
  nhlduumv : entity work.ftibladggx
    port map (wlmxcalee => et, mzbsx => xbxnou);
  
  -- Multi-driven assignments
  xpho <= '1';
end z;



-- Seed after: 5551737194949868168,5472058987609252853
