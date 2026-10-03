-- Seed: 13354944701920208972,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (bipbpzjw : buffer std_logic; vtloat : out integer; b : out integer; yzfq : linkage std_logic_vector(1 to 1));
end o;

architecture rbhbvpgu of o is
  
begin
  -- Single-driven assignments
  vtloat <= b;
  
  -- Multi-driven assignments
  bipbpzjw <= bipbpzjw;
end rbhbvpgu;

entity xdi is
  port (aayz : buffer bit_vector(0 to 4); zwlpb : buffer integer; tcdxrxw : in character);
end xdi;

library ieee;
use ieee.std_logic_1164.all;

architecture efl of xdi is
  signal lrs : std_logic_vector(1 to 1);
  signal kxoeinebn : integer;
  signal sw : std_logic_vector(1 to 1);
  signal wdksdhontd : integer;
  signal jv : integer;
  signal x : std_logic;
begin
  dcuvgw : entity work.o
    port map (bipbpzjw => x, vtloat => jv, b => wdksdhontd, yzfq => sw);
  oipxw : entity work.o
    port map (bipbpzjw => x, vtloat => kxoeinebn, b => zwlpb, yzfq => lrs);
  
  -- Single-driven assignments
  aayz <= ('1', '1', '0', '1', '1');
  
  -- Multi-driven assignments
  sw <= "U";
  x <= x;
end efl;



-- Seed after: 13244133767731627083,6140041381800297705
