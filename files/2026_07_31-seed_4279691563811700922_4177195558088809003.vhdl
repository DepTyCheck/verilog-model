-- Seed: 4279691563811700922,4177195558088809003

entity volpqbw is
  port (vczmo : in boolean_vector(1 to 2));
end volpqbw;

architecture tvi of volpqbw is
  
begin
  
end tvi;

library ieee;
use ieee.std_logic_1164.all;

entity culyl is
  port (yyht : buffer integer; vsiyjedxg : in severity_level; gxbizzh : linkage std_logic_vector(2 downto 0));
end culyl;

architecture ynmumycf of culyl is
  signal wqihuvvb : boolean_vector(1 to 2);
  signal duzl : boolean_vector(1 to 2);
begin
  vubqye : entity work.volpqbw
    port map (vczmo => duzl);
  rjk : entity work.volpqbw
    port map (vczmo => duzl);
  lhiv : entity work.volpqbw
    port map (vczmo => wqihuvvb);
  
  -- Single-driven assignments
  yyht <= 8#3_3_0_1#;
end ynmumycf;

library ieee;
use ieee.std_logic_1164.all;

entity rt is
  port (mobrvxa : buffer std_logic_vector(1 to 1); zubzrf : out real; nzo : buffer std_logic_vector(2 downto 1));
end rt;

library ieee;
use ieee.std_logic_1164.all;

architecture j of rt is
  signal wnaodrwnz : integer;
  signal hvxpshh : std_logic_vector(2 downto 0);
  signal lzcfhwtybk : severity_level;
  signal cdrcpvallm : integer;
begin
  g : entity work.culyl
    port map (yyht => cdrcpvallm, vsiyjedxg => lzcfhwtybk, gxbizzh => hvxpshh);
  nvofyucxp : entity work.culyl
    port map (yyht => wnaodrwnz, vsiyjedxg => lzcfhwtybk, gxbizzh => hvxpshh);
  
  -- Single-driven assignments
  lzcfhwtybk <= NOTE;
  zubzrf <= 8#4_0_6.4050#;
  
  -- Multi-driven assignments
  nzo <= ('U', 'W');
  nzo <= nzo;
end j;



-- Seed after: 15190055537659846779,4177195558088809003
