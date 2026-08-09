-- Seed: 12715940623415092561,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity hgsil is
  port (nelrzd : buffer time; mwnnuhiui : buffer integer; komqubf : in std_logic_vector(0 to 2); mmnckhlqtn : inout real);
end hgsil;

architecture bnwjyws of hgsil is
  
begin
  -- Single-driven assignments
  mmnckhlqtn <= 4_1_1.44;
end bnwjyws;

library ieee;
use ieee.std_logic_1164.all;

entity zhixtec is
  port (epngztnyh : linkage std_logic);
end zhixtec;

library ieee;
use ieee.std_logic_1164.all;

architecture szls of zhixtec is
  signal fbdx : real;
  signal lugrzt : std_logic_vector(0 to 2);
  signal qvh : integer;
  signal xfbybpuhz : time;
  signal wncenu : real;
  signal qufj : std_logic_vector(0 to 2);
  signal k : integer;
  signal hcj : time;
  signal jbozaeb : real;
  signal vekcr : std_logic_vector(0 to 2);
  signal koc : integer;
  signal idd : time;
  signal klj : real;
  signal hbanps : std_logic_vector(0 to 2);
  signal slu : integer;
  signal kvpkwpg : time;
begin
  u : entity work.hgsil
    port map (nelrzd => kvpkwpg, mwnnuhiui => slu, komqubf => hbanps, mmnckhlqtn => klj);
  yimuvwvelp : entity work.hgsil
    port map (nelrzd => idd, mwnnuhiui => koc, komqubf => vekcr, mmnckhlqtn => jbozaeb);
  lqpv : entity work.hgsil
    port map (nelrzd => hcj, mwnnuhiui => k, komqubf => qufj, mmnckhlqtn => wncenu);
  rdpbtxbxv : entity work.hgsil
    port map (nelrzd => xfbybpuhz, mwnnuhiui => qvh, komqubf => lugrzt, mmnckhlqtn => fbdx);
  
  -- Multi-driven assignments
  lugrzt <= "XHW";
  hbanps <= lugrzt;
  hbanps <= "0ZW";
  lugrzt <= vekcr;
end szls;

entity ysofic is
  port (fcpml : inout bit);
end ysofic;

library ieee;
use ieee.std_logic_1164.all;

architecture bsor of ysofic is
  signal xdf : real;
  signal jixy : std_logic_vector(0 to 2);
  signal lqw : integer;
  signal zlfbfjpd : time;
begin
  icqqxyr : entity work.hgsil
    port map (nelrzd => zlfbfjpd, mwnnuhiui => lqw, komqubf => jixy, mmnckhlqtn => xdf);
  
  -- Single-driven assignments
  fcpml <= fcpml;
  
  -- Multi-driven assignments
  jixy <= jixy;
  jixy <= ('X', '-', 'L');
  jixy <= ('U', 'W', '1');
end bsor;

library ieee;
use ieee.std_logic_1164.all;

entity es is
  port (kzalqm : in integer; bdsxftwmha : linkage std_logic);
end es;

library ieee;
use ieee.std_logic_1164.all;

architecture tfywp of es is
  signal zucqfdn : std_logic;
  signal mtxfbrli : real;
  signal alcmkrbvcz : std_logic_vector(0 to 2);
  signal bar : integer;
  signal amppir : time;
begin
  pynnvjva : entity work.hgsil
    port map (nelrzd => amppir, mwnnuhiui => bar, komqubf => alcmkrbvcz, mmnckhlqtn => mtxfbrli);
  sdvgb : entity work.zhixtec
    port map (epngztnyh => zucqfdn);
  
  -- Multi-driven assignments
  alcmkrbvcz <= ('L', 'U', 'Z');
end tfywp;



-- Seed after: 10712860547686953536,10871023049702252113
