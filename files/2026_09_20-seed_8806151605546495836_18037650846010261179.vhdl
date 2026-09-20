-- Seed: 8806151605546495836,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity klxfccnl is
  port (oifd : inout std_logic; ncjunrm : linkage std_logic_vector(4 downto 0); uwk : out integer_vector(2 to 3));
end klxfccnl;

architecture lrliavrrgq of klxfccnl is
  
begin
  -- Single-driven assignments
  uwk <= (8#70#, 4011);
  
  -- Multi-driven assignments
  oifd <= oifd;
  oifd <= 'H';
end lrliavrrgq;

library ieee;
use ieee.std_logic_1164.all;

entity cvuhtcytp is
  port (pdx : inout std_logic);
end cvuhtcytp;

library ieee;
use ieee.std_logic_1164.all;

architecture qeuzoa of cvuhtcytp is
  signal qpzumueh : integer_vector(2 to 3);
  signal qkgkr : std_logic_vector(4 downto 0);
  signal r : std_logic;
begin
  upkue : entity work.klxfccnl
    port map (oifd => r, ncjunrm => qkgkr, uwk => qpzumueh);
  
  -- Multi-driven assignments
  pdx <= pdx;
  r <= '0';
end qeuzoa;



-- Seed after: 4734938688338646586,18037650846010261179
