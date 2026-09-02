-- Seed: 12404042406135504254,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity now is
  port (fqzqbnsmdg : linkage std_logic_vector(0 to 4); nzk : inout time; rtglkreo : inout boolean);
end now;

architecture b of now is
  
begin
  
end b;

entity ebpsj is
  port (enyglzo : in real);
end ebpsj;

library ieee;
use ieee.std_logic_1164.all;

architecture kdtqbmmazv of ebpsj is
  signal xlpk : boolean;
  signal zvjdjknzw : time;
  signal mgpktyllc : boolean;
  signal urk : time;
  signal uhptgo : boolean;
  signal d : time;
  signal esujnn : std_logic_vector(0 to 4);
  signal uugvnl : boolean;
  signal us : time;
  signal zsvzuqt : std_logic_vector(0 to 4);
begin
  baxesjhmx : entity work.now
    port map (fqzqbnsmdg => zsvzuqt, nzk => us, rtglkreo => uugvnl);
  f : entity work.now
    port map (fqzqbnsmdg => esujnn, nzk => d, rtglkreo => uhptgo);
  elmcpgqw : entity work.now
    port map (fqzqbnsmdg => esujnn, nzk => urk, rtglkreo => mgpktyllc);
  qffm : entity work.now
    port map (fqzqbnsmdg => zsvzuqt, nzk => zvjdjknzw, rtglkreo => xlpk);
  
  -- Multi-driven assignments
  zsvzuqt <= ('0', 'L', 'X', 'Z', 'L');
end kdtqbmmazv;

entity zy is
  port (z : inout integer_vector(0 downto 1); zxesqa : linkage integer);
end zy;

architecture bvrvsngir of zy is
  signal rexkk : real;
begin
  yuwcogxy : entity work.ebpsj
    port map (enyglzo => rexkk);
  
  -- Single-driven assignments
  z <= (others => 0);
end bvrvsngir;

library ieee;
use ieee.std_logic_1164.all;

entity ttf is
  port (kkctzd : out integer; pobfwz : out time; bxrul : in std_logic_vector(0 downto 3));
end ttf;

library ieee;
use ieee.std_logic_1164.all;

architecture lqixe of ttf is
  signal qxasarnlc : boolean;
  signal coortga : std_logic_vector(0 to 4);
  signal yekhxt : boolean;
  signal hcjxep : time;
  signal hxhxcn : std_logic_vector(0 to 4);
begin
  ksfzt : entity work.now
    port map (fqzqbnsmdg => hxhxcn, nzk => hcjxep, rtglkreo => yekhxt);
  nra : entity work.now
    port map (fqzqbnsmdg => coortga, nzk => pobfwz, rtglkreo => qxasarnlc);
  
  -- Single-driven assignments
  kkctzd <= 8#1_5#;
  
  -- Multi-driven assignments
  hxhxcn <= hxhxcn;
  coortga <= coortga;
  hxhxcn <= "ZX-LZ";
  coortga <= hxhxcn;
end lqixe;



-- Seed after: 16863617761331920325,3400751927341804175
