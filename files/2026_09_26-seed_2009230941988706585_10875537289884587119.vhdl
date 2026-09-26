-- Seed: 2009230941988706585,10875537289884587119

entity mrhp is
  port (g : inout time; cpptjudwl : out boolean; lhktjsgi : buffer character);
end mrhp;

architecture j of mrhp is
  
begin
  -- Single-driven assignments
  lhktjsgi <= lhktjsgi;
  g <= g;
  cpptjudwl <= FALSE;
end j;

library ieee;
use ieee.std_logic_1164.all;

entity cuvi is
  port (dxgee : in integer; anpeywj : buffer std_logic_vector(0 to 4));
end cuvi;

architecture vucpfxpw of cuvi is
  signal jf : character;
  signal t : boolean;
  signal vmzjnqjgt : time;
begin
  ytsjbu : entity work.mrhp
    port map (g => vmzjnqjgt, cpptjudwl => t, lhktjsgi => jf);
  
  -- Multi-driven assignments
  anpeywj <= ('0', '-', 'W', '-', 'X');
end vucpfxpw;

entity bdpeqvnub is
  port (lwbf : out time);
end bdpeqvnub;

library ieee;
use ieee.std_logic_1164.all;

architecture sd of bdpeqvnub is
  signal cvuzynrsup : character;
  signal qvizyshgtj : boolean;
  signal hjrdfkhhl : std_logic_vector(0 to 4);
  signal opaovnurf : integer;
begin
  h : entity work.cuvi
    port map (dxgee => opaovnurf, anpeywj => hjrdfkhhl);
  lkpaioyc : entity work.cuvi
    port map (dxgee => opaovnurf, anpeywj => hjrdfkhhl);
  cygfnp : entity work.mrhp
    port map (g => lwbf, cpptjudwl => qvizyshgtj, lhktjsgi => cvuzynrsup);
  
  -- Single-driven assignments
  opaovnurf <= 2#1#;
  
  -- Multi-driven assignments
  hjrdfkhhl <= hjrdfkhhl;
end sd;



-- Seed after: 16119919328209777416,10875537289884587119
