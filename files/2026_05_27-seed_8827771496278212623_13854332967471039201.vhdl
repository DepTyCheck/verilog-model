-- Seed: 8827771496278212623,13854332967471039201

library ieee;
use ieee.std_logic_1164.all;

entity akta is
  port (ejsvypcd : buffer std_logic_vector(0 to 1); d : in std_logic; glicw : out real_vector(1 to 1); nkfeu : buffer boolean);
end akta;



architecture qzywwy of akta is
  
begin
  
end qzywwy;



entity ryvx is
  port (nygnqxqh : buffer time_vector(4 to 2));
end ryvx;

library ieee;
use ieee.std_logic_1164.all;

architecture c of ryvx is
  signal tukbldfqld : boolean;
  signal shcfnfz : real_vector(1 to 1);
  signal scfswgwk : std_logic;
  signal bs : boolean;
  signal pwly : real_vector(1 to 1);
  signal ha : std_logic;
  signal dpma : std_logic_vector(0 to 1);
begin
  km : entity work.akta
    port map (ejsvypcd => dpma, d => ha, glicw => pwly, nkfeu => bs);
  ht : entity work.akta
    port map (ejsvypcd => dpma, d => scfswgwk, glicw => shcfnfz, nkfeu => tukbldfqld);
end c;



-- Seed after: 219404698836988955,13854332967471039201
