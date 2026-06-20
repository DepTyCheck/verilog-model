-- Seed: 1883383676082502381,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity odvyahmky is
  port (kyzgyqo : buffer std_logic_vector(1 downto 3); fhzmjionf : buffer std_logic);
end odvyahmky;

architecture ieslgub of odvyahmky is
  
begin
  -- Multi-driven assignments
  fhzmjionf <= 'Z';
  fhzmjionf <= '1';
  fhzmjionf <= 'W';
  fhzmjionf <= '0';
end ieslgub;

entity fwwrrwfqhf is
  port (oa : out string(4 to 3));
end fwwrrwfqhf;

library ieee;
use ieee.std_logic_1164.all;

architecture gofyehst of fwwrrwfqhf is
  signal fsh : std_logic;
  signal anldgtonh : std_logic_vector(1 downto 3);
begin
  unoenfcyfc : entity work.odvyahmky
    port map (kyzgyqo => anldgtonh, fhzmjionf => fsh);
  
  -- Single-driven assignments
  oa <= (others => ' ');
end gofyehst;



-- Seed after: 14761968852998026693,17924494779688682807
