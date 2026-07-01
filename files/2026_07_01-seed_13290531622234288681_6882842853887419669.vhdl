-- Seed: 13290531622234288681,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity nrrfyzdg is
  port (iwds : in real; nly : linkage std_logic_vector(3 downto 2); rwk : inout boolean_vector(1 downto 1); qukbvsk : linkage real);
end nrrfyzdg;

architecture wfesxb of nrrfyzdg is
  
begin
  -- Single-driven assignments
  rwk <= (others => TRUE);
end wfesxb;

entity wvwz is
  port (iekoe : linkage time; nwcvrxg : inout integer; ogcdjwm : linkage real; yzkm : buffer character);
end wvwz;

library ieee;
use ieee.std_logic_1164.all;

architecture qz of wvwz is
  signal rojzmmxd : boolean_vector(1 downto 1);
  signal tveenbde : std_logic_vector(3 downto 2);
  signal x : real;
  signal sovm : boolean_vector(1 downto 1);
  signal hssq : std_logic_vector(3 downto 2);
  signal agsmg : boolean_vector(1 downto 1);
  signal yw : std_logic_vector(3 downto 2);
  signal sc : real;
  signal rkerqfc : boolean_vector(1 downto 1);
  signal qlioap : std_logic_vector(3 downto 2);
  signal bqbroa : real;
begin
  rdscs : entity work.nrrfyzdg
    port map (iwds => bqbroa, nly => qlioap, rwk => rkerqfc, qukbvsk => bqbroa);
  gssbqwp : entity work.nrrfyzdg
    port map (iwds => sc, nly => yw, rwk => agsmg, qukbvsk => ogcdjwm);
  vewuityb : entity work.nrrfyzdg
    port map (iwds => sc, nly => hssq, rwk => sovm, qukbvsk => x);
  wnlmzxxd : entity work.nrrfyzdg
    port map (iwds => bqbroa, nly => tveenbde, rwk => rojzmmxd, qukbvsk => sc);
  
  -- Single-driven assignments
  yzkm <= 'w';
  nwcvrxg <= 2;
  
  -- Multi-driven assignments
  hssq <= ('-', 'W');
  qlioap <= ('Z', 'W');
end qz;



-- Seed after: 6751537028026672600,6882842853887419669
