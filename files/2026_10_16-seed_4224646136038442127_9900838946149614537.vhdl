-- Seed: 4224646136038442127,9900838946149614537

entity tgllfew is
  port (ldsuswmsa : linkage time; yofbxz : in integer; t : out time);
end tgllfew;

architecture ukkwid of tgllfew is
  
begin
  -- Single-driven assignments
  t <= 16#F08# fs;
end ukkwid;

library ieee;
use ieee.std_logic_1164.all;

entity euxmqc is
  port (ycvf : buffer std_logic; mnwofy : buffer integer; kjo : linkage std_logic);
end euxmqc;

architecture zraa of euxmqc is
  signal jtt : time;
  signal qhziwuh : integer;
  signal z : time;
  signal kpzegltt : time;
  signal pdi : time;
  signal cpiz : time;
  signal yrdnrqd : time;
  signal mb : time;
  signal urd : integer;
  signal lse : time;
begin
  egvop : entity work.tgllfew
    port map (ldsuswmsa => lse, yofbxz => urd, t => mb);
  qcvypsf : entity work.tgllfew
    port map (ldsuswmsa => yrdnrqd, yofbxz => urd, t => cpiz);
  siudt : entity work.tgllfew
    port map (ldsuswmsa => pdi, yofbxz => mnwofy, t => kpzegltt);
  mh : entity work.tgllfew
    port map (ldsuswmsa => z, yofbxz => qhziwuh, t => jtt);
  
  -- Single-driven assignments
  mnwofy <= 16#E#;
  qhziwuh <= urd;
  urd <= mnwofy;
  
  -- Multi-driven assignments
  ycvf <= ycvf;
end zraa;



-- Seed after: 8629668744562620836,9900838946149614537
