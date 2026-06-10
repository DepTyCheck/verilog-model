-- Seed: 2462718553394209497,5415160250146859793



entity njb is
  port (jzddbdfkb : linkage integer; avatx : inout integer);
end njb;



architecture fern of njb is
  
begin
  
end fern;



entity hrmjqpec is
  port (lrwooax : out integer_vector(1 downto 3); tckhuaygc : in integer);
end hrmjqpec;



architecture nm of hrmjqpec is
  signal w : integer;
  signal qgioqs : integer;
  signal c : integer;
begin
  nekgiahwla : entity work.njb
    port map (jzddbdfkb => tckhuaygc, avatx => c);
  t : entity work.njb
    port map (jzddbdfkb => qgioqs, avatx => qgioqs);
  elmliaabfi : entity work.njb
    port map (jzddbdfkb => c, avatx => w);
end nm;

library ieee;
use ieee.std_logic_1164.all;

entity sgigqubc is
  port (vpyibo : buffer integer; cljkj : linkage std_logic; kqah : buffer std_logic_vector(3 downto 2); dmedjoliyi : out real);
end sgigqubc;



architecture uurltzvlmf of sgigqubc is
  signal csbg : integer;
  signal nj : integer_vector(1 downto 3);
  signal yxkhv : integer;
begin
  eyounu : entity work.njb
    port map (jzddbdfkb => yxkhv, avatx => vpyibo);
  gikzwxclm : entity work.hrmjqpec
    port map (lrwooax => nj, tckhuaygc => vpyibo);
  ra : entity work.njb
    port map (jzddbdfkb => vpyibo, avatx => csbg);
end uurltzvlmf;



-- Seed after: 15831812714214699732,5415160250146859793
