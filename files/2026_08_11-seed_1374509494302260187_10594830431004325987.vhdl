-- Seed: 1374509494302260187,10594830431004325987

entity ixlpe is
  port (jejuwfy : buffer time; a : inout real);
end ixlpe;

architecture tbzqxq of ixlpe is
  
begin
  
end tbzqxq;

entity eiovrazp is
  port (owyclnqby : linkage real);
end eiovrazp;

architecture ntru of eiovrazp is
  signal q : real;
  signal wlbs : time;
  signal qa : real;
  signal gzbr : time;
  signal lvzjjjt : real;
  signal bdbuguz : time;
begin
  kvsstnxaz : entity work.ixlpe
    port map (jejuwfy => bdbuguz, a => lvzjjjt);
  nsb : entity work.ixlpe
    port map (jejuwfy => gzbr, a => qa);
  naix : entity work.ixlpe
    port map (jejuwfy => wlbs, a => q);
end ntru;

library ieee;
use ieee.std_logic_1164.all;

entity lgtowcmjmf is
  port (aqr : buffer severity_level; txdwi : buffer integer; yimme : out std_logic_vector(1 to 2));
end lgtowcmjmf;

architecture wgretpv of lgtowcmjmf is
  signal pnaodtabs : real;
  signal eq : real;
begin
  rzgffvb : entity work.eiovrazp
    port map (owyclnqby => eq);
  cnpd : entity work.eiovrazp
    port map (owyclnqby => pnaodtabs);
  
  -- Single-driven assignments
  aqr <= FAILURE;
  txdwi <= 1;
  
  -- Multi-driven assignments
  yimme <= ('H', 'H');
  yimme <= ('-', 'X');
  yimme <= "11";
end wgretpv;



-- Seed after: 13348532901490922006,10594830431004325987
