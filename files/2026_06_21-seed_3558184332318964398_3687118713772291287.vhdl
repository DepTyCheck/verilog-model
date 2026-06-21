-- Seed: 3558184332318964398,3687118713772291287

entity fuwo is
  port (gywkhao : linkage severity_level);
end fuwo;

architecture ule of fuwo is
  
begin
  
end ule;

library ieee;
use ieee.std_logic_1164.all;

entity zors is
  port (fnluqq : linkage std_logic_vector(3 to 1));
end zors;

architecture muwinuyi of zors is
  signal qaistzh : severity_level;
  signal xhdfrfe : severity_level;
  signal urbcgeth : severity_level;
  signal ruoonyctw : severity_level;
begin
  oazv : entity work.fuwo
    port map (gywkhao => ruoonyctw);
  ygcsp : entity work.fuwo
    port map (gywkhao => urbcgeth);
  ae : entity work.fuwo
    port map (gywkhao => xhdfrfe);
  xcukkfld : entity work.fuwo
    port map (gywkhao => qaistzh);
end muwinuyi;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (eezsp : buffer std_logic_vector(2 to 4); hpbcwrwh : in string(3 downto 3); nuqackgy : in std_logic_vector(0 downto 4));
end w;

architecture st of w is
  signal frhyeztjz : severity_level;
  signal oxfxao : severity_level;
begin
  sxkaznuybo : entity work.zors
    port map (fnluqq => nuqackgy);
  tlgbnc : entity work.fuwo
    port map (gywkhao => oxfxao);
  jp : entity work.fuwo
    port map (gywkhao => frhyeztjz);
  
  -- Multi-driven assignments
  eezsp <= "1UZ";
  eezsp <= ('X', 'U', 'H');
end st;



-- Seed after: 17916446849731252520,3687118713772291287
