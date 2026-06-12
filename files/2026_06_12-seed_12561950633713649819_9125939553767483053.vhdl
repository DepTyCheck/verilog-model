-- Seed: 12561950633713649819,9125939553767483053



entity klo is
  port (dmfxttrma : buffer integer; bfwa : buffer time; jnqu : buffer boolean_vector(0 downto 2); pfi : linkage severity_level);
end klo;



architecture iomomp of klo is
  
begin
  
end iomomp;

library ieee;
use ieee.std_logic_1164.all;

entity gvxsdxq is
  port (rqcn : buffer severity_level; relbprmg : inout std_logic_vector(2 to 4); yuiqpn : out bit);
end gvxsdxq;



architecture lhyt of gvxsdxq is
  signal p : severity_level;
  signal jjclrpm : boolean_vector(0 downto 2);
  signal istzpm : time;
  signal zzhfx : integer;
begin
  rilq : entity work.klo
    port map (dmfxttrma => zzhfx, bfwa => istzpm, jnqu => jjclrpm, pfi => p);
end lhyt;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (kjwzk : inout std_logic; utko : buffer std_logic; vqzfxfjx : buffer integer; geuwf : buffer boolean);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture wedxbrc of o is
  signal florpxaow : bit;
  signal lyeemgevph : std_logic_vector(2 to 4);
  signal ojrlnolnf : severity_level;
  signal esigbzo : severity_level;
  signal db : boolean_vector(0 downto 2);
  signal aetj : time;
  signal bposy : integer;
begin
  wpueyvel : entity work.klo
    port map (dmfxttrma => bposy, bfwa => aetj, jnqu => db, pfi => esigbzo);
  mosd : entity work.gvxsdxq
    port map (rqcn => ojrlnolnf, relbprmg => lyeemgevph, yuiqpn => florpxaow);
end wedxbrc;

library ieee;
use ieee.std_logic_1164.all;

entity ggljo is
  port (j : out bit_vector(4 to 2); evsdvpxf : in std_logic; di : out real);
end ggljo;

library ieee;
use ieee.std_logic_1164.all;

architecture lnfltap of ggljo is
  signal yqrpzicoah : boolean;
  signal a : integer;
  signal m : severity_level;
  signal eaaohp : boolean_vector(0 downto 2);
  signal hqpwaw : time;
  signal zkamgfe : integer;
  signal hw : boolean;
  signal kmsvogj : integer;
  signal b : std_logic;
begin
  uk : entity work.o
    port map (kjwzk => b, utko => b, vqzfxfjx => kmsvogj, geuwf => hw);
  phsaxeiq : entity work.klo
    port map (dmfxttrma => zkamgfe, bfwa => hqpwaw, jnqu => eaaohp, pfi => m);
  v : entity work.o
    port map (kjwzk => b, utko => b, vqzfxfjx => a, geuwf => yqrpzicoah);
end lnfltap;



-- Seed after: 3737622540161186080,9125939553767483053
