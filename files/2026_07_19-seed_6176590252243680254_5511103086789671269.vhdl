-- Seed: 6176590252243680254,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity wue is
  port (cfrgewgcq : in std_logic_vector(3 to 2); xpcx : inout time; hk : out real);
end wue;

architecture usalyi of wue is
  
begin
  -- Single-driven assignments
  hk <= hk;
  xpcx <= xpcx;
end usalyi;

entity phayu is
  port (xne : out real);
end phayu;

library ieee;
use ieee.std_logic_1164.all;

architecture iccxcu of phayu is
  signal ekfobmcfs : real;
  signal htolbxehmu : time;
  signal bpfdso : time;
  signal w : std_logic_vector(3 to 2);
  signal qntei : real;
  signal zxg : time;
  signal jnglrfzm : real;
  signal faxgld : time;
  signal nvmuumzub : std_logic_vector(3 to 2);
begin
  pfj : entity work.wue
    port map (cfrgewgcq => nvmuumzub, xpcx => faxgld, hk => jnglrfzm);
  r : entity work.wue
    port map (cfrgewgcq => nvmuumzub, xpcx => zxg, hk => qntei);
  h : entity work.wue
    port map (cfrgewgcq => w, xpcx => bpfdso, hk => xne);
  mu : entity work.wue
    port map (cfrgewgcq => w, xpcx => htolbxehmu, hk => ekfobmcfs);
  
  -- Multi-driven assignments
  nvmuumzub <= nvmuumzub;
  nvmuumzub <= (others => '0');
  w <= (others => '0');
end iccxcu;

entity imda is
  port (mvya : buffer time);
end imda;

library ieee;
use ieee.std_logic_1164.all;

architecture n of imda is
  signal tpfxyfz : real;
  signal bbuzaqmo : std_logic_vector(3 to 2);
  signal qipgigtdjq : real;
  signal kcudlwl : time;
  signal odzjz : real;
  signal twwi : time;
  signal pdnqtsku : std_logic_vector(3 to 2);
begin
  yiqkd : entity work.wue
    port map (cfrgewgcq => pdnqtsku, xpcx => twwi, hk => odzjz);
  oqxkzgxrzt : entity work.wue
    port map (cfrgewgcq => pdnqtsku, xpcx => kcudlwl, hk => qipgigtdjq);
  hoygiaw : entity work.wue
    port map (cfrgewgcq => bbuzaqmo, xpcx => mvya, hk => tpfxyfz);
end n;



-- Seed after: 4011598739174764547,5511103086789671269
