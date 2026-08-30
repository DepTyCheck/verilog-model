-- Seed: 16118451092158081391,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity qe is
  port (ugdohv : inout std_logic_vector(3 to 1); oxxldxwnlk : out std_logic_vector(1 downto 3); zearsvlwuo : in time);
end qe;

architecture nievpcaxcj of qe is
  
begin
  -- Multi-driven assignments
  oxxldxwnlk <= "";
  ugdohv <= "";
  oxxldxwnlk <= (others => '0');
end nievpcaxcj;

library ieee;
use ieee.std_logic_1164.all;

entity rvp is
  port (vxtmz : buffer std_logic; jgrtpuvha : in character);
end rvp;

library ieee;
use ieee.std_logic_1164.all;

architecture q of rvp is
  signal r : time;
  signal rbp : std_logic_vector(1 downto 3);
  signal txmf : std_logic_vector(3 to 1);
  signal oaxypz : time;
  signal xamjuaxgkq : std_logic_vector(1 downto 3);
  signal h : std_logic_vector(3 to 1);
  signal jzlsmyyqz : time;
  signal tweqpmo : std_logic_vector(3 to 1);
begin
  xoxmwb : entity work.qe
    port map (ugdohv => tweqpmo, oxxldxwnlk => tweqpmo, zearsvlwuo => jzlsmyyqz);
  ynxwvp : entity work.qe
    port map (ugdohv => h, oxxldxwnlk => tweqpmo, zearsvlwuo => jzlsmyyqz);
  vasugx : entity work.qe
    port map (ugdohv => tweqpmo, oxxldxwnlk => xamjuaxgkq, zearsvlwuo => oaxypz);
  mextaqrhjp : entity work.qe
    port map (ugdohv => txmf, oxxldxwnlk => rbp, zearsvlwuo => r);
  
  -- Multi-driven assignments
  vxtmz <= '1';
end q;



-- Seed after: 15690689170363534886,4080032123900078489
