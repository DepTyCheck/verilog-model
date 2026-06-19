-- Seed: 12498185534791937687,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity wo is
  port (q : in std_logic_vector(0 to 4); yxe : inout real);
end wo;

architecture zeziqo of wo is
  
begin
  -- Single-driven assignments
  yxe <= 30.14;
end zeziqo;

library ieee;
use ieee.std_logic_1164.all;

entity ezer is
  port (p : out std_logic; pkh : in time);
end ezer;

library ieee;
use ieee.std_logic_1164.all;

architecture bjccynd of ezer is
  signal reo : real;
  signal qj : std_logic_vector(0 to 4);
  signal rtullxgf : real;
  signal yviw : std_logic_vector(0 to 4);
  signal snorbnvu : real;
  signal atvkqhthc : std_logic_vector(0 to 4);
begin
  wt : entity work.wo
    port map (q => atvkqhthc, yxe => snorbnvu);
  ljuvax : entity work.wo
    port map (q => yviw, yxe => rtullxgf);
  kdhywbzknz : entity work.wo
    port map (q => qj, yxe => reo);
end bjccynd;

entity ktactcu is
  port (bkwhmuhrw : out bit_vector(3 to 2));
end ktactcu;

library ieee;
use ieee.std_logic_1164.all;

architecture kdguluc of ktactcu is
  signal ncdunaoyf : real;
  signal o : real;
  signal iysfgfd : std_logic_vector(0 to 4);
begin
  itdk : entity work.wo
    port map (q => iysfgfd, yxe => o);
  tk : entity work.wo
    port map (q => iysfgfd, yxe => ncdunaoyf);
  
  -- Single-driven assignments
  bkwhmuhrw <= (others => '0');
  
  -- Multi-driven assignments
  iysfgfd <= "1WZU-";
  iysfgfd <= ('H', '1', '-', '1', '-');
  iysfgfd <= "LLLZW";
  iysfgfd <= "HLWHW";
end kdguluc;

entity kbmj is
  port (avtw : buffer real; lucxre : linkage boolean; kdzawn : inout integer; mohpudkjvx : linkage real);
end kbmj;

architecture zp of kbmj is
  
begin
  -- Single-driven assignments
  avtw <= 16#D.1#;
  kdzawn <= 2#0_0_0_1#;
end zp;



-- Seed after: 14833861640961952906,3108530264173481209
