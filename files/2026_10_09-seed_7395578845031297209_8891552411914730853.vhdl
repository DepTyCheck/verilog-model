-- Seed: 7395578845031297209,8891552411914730853

entity ficyd is
  port (jukmdixi : in time; yifctsebo : linkage boolean; zdlw : linkage time);
end ficyd;

architecture qzgn of ficyd is
  
begin
  
end qzgn;

entity viyyk is
  port (xlumr : buffer time; mmcbctbm : out real_vector(3 downto 3));
end viyyk;

architecture whyghipbo of viyyk is
  signal cxhdvap : time;
  signal ilgg : boolean;
  signal ssozwdxj : time;
  signal e : boolean;
  signal vkdtytltq : time;
begin
  kgzelhs : entity work.ficyd
    port map (jukmdixi => vkdtytltq, yifctsebo => e, zdlw => ssozwdxj);
  wpbktmlp : entity work.ficyd
    port map (jukmdixi => ssozwdxj, yifctsebo => ilgg, zdlw => cxhdvap);
  
  -- Single-driven assignments
  vkdtytltq <= xlumr;
  xlumr <= xlumr;
  mmcbctbm <= mmcbctbm;
end whyghipbo;

library ieee;
use ieee.std_logic_1164.all;

entity kbmazwsoj is
  port (uryn : buffer time; tnrdme : out std_logic; wlbgee : in time);
end kbmazwsoj;

architecture rl of kbmazwsoj is
  signal wdrmqdipl : boolean;
  signal dltg : time;
  signal wglldox : real_vector(3 downto 3);
begin
  dolmvpwzdq : entity work.viyyk
    port map (xlumr => uryn, mmcbctbm => wglldox);
  myia : entity work.ficyd
    port map (jukmdixi => dltg, yifctsebo => wdrmqdipl, zdlw => dltg);
  
  -- Multi-driven assignments
  tnrdme <= tnrdme;
  tnrdme <= 'L';
  tnrdme <= tnrdme;
end rl;



-- Seed after: 13930486653462743678,8891552411914730853
