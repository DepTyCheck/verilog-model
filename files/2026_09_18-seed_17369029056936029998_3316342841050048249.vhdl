-- Seed: 17369029056936029998,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity sjgmaix is
  port (yshbmsm : out std_logic_vector(0 downto 1); prhe : out std_logic; zcg : inout time; q : inout time_vector(4 downto 4));
end sjgmaix;

architecture ktxmr of sjgmaix is
  
begin
  -- Single-driven assignments
  q <= (others => 0 hr);
  zcg <= zcg;
  
  -- Multi-driven assignments
  yshbmsm <= (others => '0');
  prhe <= 'H';
end ktxmr;

entity iyyd is
  port (supacbed : buffer integer; agrzvzo : in severity_level);
end iyyd;

library ieee;
use ieee.std_logic_1164.all;

architecture enzrhc of iyyd is
  signal tpu : time_vector(4 downto 4);
  signal ngujtic : time;
  signal twlddu : std_logic;
  signal esps : std_logic_vector(0 downto 1);
  signal ilufkhybrd : time_vector(4 downto 4);
  signal tvfjipum : time;
  signal ftpl : std_logic;
  signal ocxuwxm : std_logic_vector(0 downto 1);
  signal opv : time_vector(4 downto 4);
  signal djdahaqm : time;
  signal mo : std_logic;
  signal xrovz : time_vector(4 downto 4);
  signal svmgcqbjq : time;
  signal btp : std_logic;
  signal jmqbz : std_logic_vector(0 downto 1);
begin
  tecqx : entity work.sjgmaix
    port map (yshbmsm => jmqbz, prhe => btp, zcg => svmgcqbjq, q => xrovz);
  ev : entity work.sjgmaix
    port map (yshbmsm => jmqbz, prhe => mo, zcg => djdahaqm, q => opv);
  ornjfmg : entity work.sjgmaix
    port map (yshbmsm => ocxuwxm, prhe => ftpl, zcg => tvfjipum, q => ilufkhybrd);
  riqjcdl : entity work.sjgmaix
    port map (yshbmsm => esps, prhe => twlddu, zcg => ngujtic, q => tpu);
  
  -- Single-driven assignments
  supacbed <= 8#6#;
  
  -- Multi-driven assignments
  ocxuwxm <= (others => '0');
  jmqbz <= jmqbz;
end enzrhc;



-- Seed after: 8506416901742259493,3316342841050048249
