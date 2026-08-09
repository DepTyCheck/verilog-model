-- Seed: 10711761147610535335,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity be is
  port (sjwwlxfpy : buffer std_logic_vector(0 downto 4); uyxy : out integer_vector(4 downto 2));
end be;

architecture ijdpilesi of be is
  
begin
  -- Multi-driven assignments
  sjwwlxfpy <= sjwwlxfpy;
  sjwwlxfpy <= sjwwlxfpy;
  sjwwlxfpy <= sjwwlxfpy;
  sjwwlxfpy <= (others => '0');
end ijdpilesi;

library ieee;
use ieee.std_logic_1164.all;

entity omaw is
  port (rzvvue : linkage std_logic_vector(2 downto 2));
end omaw;

library ieee;
use ieee.std_logic_1164.all;

architecture u of omaw is
  signal pgajw : integer_vector(4 downto 2);
  signal sttetqn : std_logic_vector(0 downto 4);
begin
  gbezurnwdn : entity work.be
    port map (sjwwlxfpy => sttetqn, uyxy => pgajw);
  
  -- Multi-driven assignments
  sttetqn <= (others => '0');
end u;

entity ric is
  port (bpehuciapi : out real_vector(4 downto 1));
end ric;

library ieee;
use ieee.std_logic_1164.all;

architecture f of ric is
  signal k : std_logic_vector(2 downto 2);
begin
  yqp : entity work.omaw
    port map (rzvvue => k);
  llsodowqj : entity work.omaw
    port map (rzvvue => k);
  
  -- Single-driven assignments
  bpehuciapi <= (16#81486.8248#, 2#1_0_0_0.0_1_0_1_1#, 22.0_1_1, 33.3_0);
  
  -- Multi-driven assignments
  k <= (others => '1');
  k <= "H";
  k <= k;
end f;



-- Seed after: 7835620988267548920,10871023049702252113
