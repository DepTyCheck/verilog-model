-- Seed: 16626768037722601074,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity agsdwjdt is
  port (fqepbvkum : linkage real; geo : linkage time; og : out std_logic; vx : out std_logic);
end agsdwjdt;

architecture eqrvhn of agsdwjdt is
  
begin
  -- Multi-driven assignments
  vx <= '0';
  og <= 'H';
  vx <= '1';
  vx <= 'W';
end eqrvhn;

entity mqgh is
  port (q : buffer time; bdkf : buffer boolean);
end mqgh;

library ieee;
use ieee.std_logic_1164.all;

architecture ifakakj of mqgh is
  signal bzc : std_logic;
  signal rh : std_logic;
  signal wbll : real;
  signal wwkqgd : time;
  signal qf : real;
  signal pjfpt : std_logic;
  signal amgtjsl : std_logic;
  signal rfvnahxe : time;
  signal vedupkzxf : real;
begin
  oqut : entity work.agsdwjdt
    port map (fqepbvkum => vedupkzxf, geo => rfvnahxe, og => amgtjsl, vx => pjfpt);
  shvqadm : entity work.agsdwjdt
    port map (fqepbvkum => qf, geo => wwkqgd, og => amgtjsl, vx => pjfpt);
  tfrnuz : entity work.agsdwjdt
    port map (fqepbvkum => wbll, geo => q, og => rh, vx => bzc);
  
  -- Single-driven assignments
  bdkf <= TRUE;
end ifakakj;



-- Seed after: 7761394496562421026,17047277710231705797
