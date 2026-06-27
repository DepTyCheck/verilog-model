-- Seed: 15206587325105926921,4860866131898729603

entity tugla is
  port (ealnyqx : out boolean_vector(3 to 0); knpwhg : out real; lwmbqansvq : linkage time);
end tugla;

architecture iwyhvtx of tugla is
  
begin
  -- Single-driven assignments
  knpwhg <= 16#8E.ED6E0#;
  ealnyqx <= (others => TRUE);
end iwyhvtx;

library ieee;
use ieee.std_logic_1164.all;

entity znhobk is
  port (ezohpo : in std_logic; zelcojcgk : out integer_vector(3 to 2); raw : in std_logic; rsaz : inout real);
end znhobk;

architecture klb of znhobk is
  signal dfvq : time;
  signal kvrvbybys : boolean_vector(3 to 0);
  signal zcos : time;
  signal mutbziq : real;
  signal elsnxi : boolean_vector(3 to 0);
  signal zxxhngz : time;
  signal ocr : real;
  signal yhpnxiibfy : boolean_vector(3 to 0);
  signal fdeh : time;
  signal whvk : real;
  signal rqkrcgkuz : boolean_vector(3 to 0);
begin
  qn : entity work.tugla
    port map (ealnyqx => rqkrcgkuz, knpwhg => whvk, lwmbqansvq => fdeh);
  taj : entity work.tugla
    port map (ealnyqx => yhpnxiibfy, knpwhg => ocr, lwmbqansvq => zxxhngz);
  jjsclg : entity work.tugla
    port map (ealnyqx => elsnxi, knpwhg => mutbziq, lwmbqansvq => zcos);
  i : entity work.tugla
    port map (ealnyqx => kvrvbybys, knpwhg => rsaz, lwmbqansvq => dfvq);
  
  -- Single-driven assignments
  zelcojcgk <= (others => 0);
end klb;



-- Seed after: 15914851322268358391,4860866131898729603
