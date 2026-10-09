-- Seed: 9979815585369831828,8891552411914730853

entity hri is
  port (tdvodbrjjd : inout time; cmptphe : linkage real; cw : in real);
end hri;

architecture jszp of hri is
  
begin
  -- Single-driven assignments
  tdvodbrjjd <= 2#00# ps;
end jszp;

library ieee;
use ieee.std_logic_1164.all;

entity zite is
  port (u : inout bit; cyjwkeu : linkage real; hywkiwbbaa : buffer std_logic);
end zite;

architecture i of zite is
  signal jz : real;
  signal oepo : time;
  signal jrhdnya : real;
  signal vmubsqr : real;
  signal iwwz : time;
  signal lcq : real;
  signal mhdprjx : time;
  signal qs : real;
  signal asks : time;
begin
  brccuppt : entity work.hri
    port map (tdvodbrjjd => asks, cmptphe => qs, cw => qs);
  e : entity work.hri
    port map (tdvodbrjjd => mhdprjx, cmptphe => lcq, cw => qs);
  mjkkte : entity work.hri
    port map (tdvodbrjjd => iwwz, cmptphe => vmubsqr, cw => jrhdnya);
  nklqlrccyn : entity work.hri
    port map (tdvodbrjjd => oepo, cmptphe => jrhdnya, cw => jz);
  
  -- Single-driven assignments
  u <= u;
  jz <= 16#0.15#;
  
  -- Multi-driven assignments
  hywkiwbbaa <= hywkiwbbaa;
  hywkiwbbaa <= hywkiwbbaa;
  hywkiwbbaa <= 'L';
end i;



-- Seed after: 12643772087884225881,8891552411914730853
