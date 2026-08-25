-- Seed: 16914486164876369381,13501862637168280927

entity kge is
  port (dx : buffer time; clnllp : out bit_vector(2 to 3));
end kge;

architecture sfd of kge is
  
begin
  -- Single-driven assignments
  clnllp <= clnllp;
  dx <= dx;
end sfd;

entity mlvkdylm is
  port (esn : out bit_vector(4 downto 1); ppy : in real);
end mlvkdylm;

architecture uujy of mlvkdylm is
  signal wbl : bit_vector(2 to 3);
  signal thz : time;
begin
  onn : entity work.kge
    port map (dx => thz, clnllp => wbl);
  
  -- Single-driven assignments
  esn <= ('1', '0', '1', '0');
end uujy;

library ieee;
use ieee.std_logic_1164.all;

entity cczo is
  port (iil : in bit; sliwihv : inout time; htia : inout std_logic);
end cczo;

architecture awzza of cczo is
  signal s : real;
  signal ggnkfz : bit_vector(4 downto 1);
  signal tcjn : bit_vector(2 to 3);
  signal zhrewl : bit_vector(2 to 3);
  signal efaygqre : time;
  signal kd : bit_vector(2 to 3);
  signal sqth : time;
begin
  mkkezuddv : entity work.kge
    port map (dx => sqth, clnllp => kd);
  m : entity work.kge
    port map (dx => efaygqre, clnllp => zhrewl);
  fwu : entity work.kge
    port map (dx => sliwihv, clnllp => tcjn);
  mkt : entity work.mlvkdylm
    port map (esn => ggnkfz, ppy => s);
  
  -- Single-driven assignments
  s <= 32004.321;
  
  -- Multi-driven assignments
  htia <= 'W';
  htia <= htia;
  htia <= htia;
  htia <= htia;
end awzza;

library ieee;
use ieee.std_logic_1164.all;

entity qjcryweifj is
  port (cxtegwb : out std_logic; tsyzrwfe : buffer time; mmqygvdl : buffer integer);
end qjcryweifj;

architecture qui of qjcryweifj is
  signal ikzvrnxg : bit_vector(2 to 3);
  signal iswd : time;
begin
  ceeps : entity work.kge
    port map (dx => iswd, clnllp => ikzvrnxg);
  
  -- Single-driven assignments
  mmqygvdl <= 00142;
  tsyzrwfe <= 3_1_2_1 ms;
  
  -- Multi-driven assignments
  cxtegwb <= cxtegwb;
  cxtegwb <= 'X';
end qui;



-- Seed after: 4380126501074806773,13501862637168280927
