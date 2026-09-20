-- Seed: 3452948613267692917,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity hcwmnq is
  port (g : inout std_logic_vector(3 downto 0); mnbvi : buffer std_logic_vector(2 downto 4); udbbdnvp : in real; pvqoy : buffer integer);
end hcwmnq;

architecture hjy of hcwmnq is
  
begin
  -- Multi-driven assignments
  mnbvi <= (others => '0');
end hjy;

library ieee;
use ieee.std_logic_1164.all;

entity mt is
  port (kjsz : linkage boolean; b : in integer; uvqekcavj : inout std_logic_vector(2 to 4); tdtgodzl : in integer);
end mt;

library ieee;
use ieee.std_logic_1164.all;

architecture fhhpw of mt is
  signal msvlbvixo : integer;
  signal jw : std_logic_vector(3 downto 0);
  signal qnfxr : integer;
  signal vuexnvlcg : real;
  signal db : std_logic_vector(2 downto 4);
  signal dipjm : std_logic_vector(3 downto 0);
  signal oajjsaporu : integer;
  signal qyldqz : real;
  signal fkenzwlw : std_logic_vector(2 downto 4);
  signal ry : std_logic_vector(3 downto 0);
begin
  iueoximnc : entity work.hcwmnq
    port map (g => ry, mnbvi => fkenzwlw, udbbdnvp => qyldqz, pvqoy => oajjsaporu);
  tceun : entity work.hcwmnq
    port map (g => dipjm, mnbvi => db, udbbdnvp => vuexnvlcg, pvqoy => qnfxr);
  nqxxkis : entity work.hcwmnq
    port map (g => jw, mnbvi => fkenzwlw, udbbdnvp => vuexnvlcg, pvqoy => msvlbvixo);
  
  -- Single-driven assignments
  qyldqz <= 16#B_B_A.4B6#;
  vuexnvlcg <= qyldqz;
  
  -- Multi-driven assignments
  db <= db;
  db <= fkenzwlw;
  uvqekcavj <= uvqekcavj;
  uvqekcavj <= uvqekcavj;
end fhhpw;



-- Seed after: 15397583635315718805,18037650846010261179
