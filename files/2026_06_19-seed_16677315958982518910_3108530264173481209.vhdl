-- Seed: 16677315958982518910,3108530264173481209

entity eue is
  port (d : inout string(1 to 2); lrett : linkage time);
end eue;

architecture edvn of eue is
  
begin
  -- Single-driven assignments
  d <= ('q', 'x');
end edvn;

library ieee;
use ieee.std_logic_1164.all;

entity dldf is
  port (mhq : inout std_logic_vector(4 to 4));
end dldf;

architecture wc of dldf is
  signal jbshavrb : time;
  signal qfvd : string(1 to 2);
  signal titucdtrx : time;
  signal l : string(1 to 2);
begin
  ywasrde : entity work.eue
    port map (d => l, lrett => titucdtrx);
  jsoriet : entity work.eue
    port map (d => qfvd, lrett => jbshavrb);
  
  -- Multi-driven assignments
  mhq <= "0";
  mhq <= "L";
end wc;

library ieee;
use ieee.std_logic_1164.all;

entity xmqrt is
  port ( rzuncqopoh : linkage std_logic_vector(1 to 3)
  ; ckkpheesrq : buffer real_vector(1 to 4)
  ; gnwl : buffer std_logic
  ; i : in std_logic_vector(4 to 4)
  );
end xmqrt;

architecture jhapcgynmg of xmqrt is
  
begin
  -- Multi-driven assignments
  gnwl <= '0';
  gnwl <= 'H';
  gnwl <= 'L';
end jhapcgynmg;

entity mzxghombao is
  port (shlkhmsh : out boolean; zsbrfu : out real_vector(1 to 0));
end mzxghombao;

library ieee;
use ieee.std_logic_1164.all;

architecture avdolld of mzxghombao is
  signal tvdamud : time;
  signal piplvyaq : string(1 to 2);
  signal wqsryj : time;
  signal mqmp : string(1 to 2);
  signal hdr : std_logic_vector(4 to 4);
  signal na : time;
  signal qhrr : string(1 to 2);
begin
  fnx : entity work.eue
    port map (d => qhrr, lrett => na);
  kt : entity work.dldf
    port map (mhq => hdr);
  zpqxqcn : entity work.eue
    port map (d => mqmp, lrett => wqsryj);
  tsdyol : entity work.eue
    port map (d => piplvyaq, lrett => tvdamud);
  
  -- Single-driven assignments
  shlkhmsh <= TRUE;
  zsbrfu <= (others => 0.0);
  
  -- Multi-driven assignments
  hdr <= "-";
  hdr <= "1";
end avdolld;



-- Seed after: 11029128203858649133,3108530264173481209
