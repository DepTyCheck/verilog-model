-- Seed: 6397617264509957238,5805648483995786113

entity qa is
  port (tkecmam : out bit);
end qa;

architecture yygftkh of qa is
  
begin
  -- Single-driven assignments
  tkecmam <= '1';
end yygftkh;

entity nqm is
  port (slspflyw : in integer; bkgrx : out real; ynvlfgair : linkage string(2 to 2); quuy : buffer real);
end nqm;

architecture opxyosdpyl of nqm is
  signal djigap : bit;
  signal sytqm : bit;
  signal weglphmo : bit;
  signal mw : bit;
begin
  hoiegtlj : entity work.qa
    port map (tkecmam => mw);
  mnw : entity work.qa
    port map (tkecmam => weglphmo);
  wkrbg : entity work.qa
    port map (tkecmam => sytqm);
  pwjbicysp : entity work.qa
    port map (tkecmam => djigap);
  
  -- Single-driven assignments
  quuy <= 2.324;
  bkgrx <= quuy;
end opxyosdpyl;

library ieee;
use ieee.std_logic_1164.all;

entity biyzwwdvk is
  port (jrdyrfynyh : in std_logic);
end biyzwwdvk;

architecture yo of biyzwwdvk is
  signal mdz : bit;
  signal gpwqv : bit;
  signal n : bit;
begin
  np : entity work.qa
    port map (tkecmam => n);
  ch : entity work.qa
    port map (tkecmam => gpwqv);
  nlyyyebj : entity work.qa
    port map (tkecmam => mdz);
end yo;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (crnvpecuiv : buffer bit_vector(4 downto 4); z : out real; qv : out std_logic_vector(4 downto 1));
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture wghsetws of h is
  signal iqmcqq : bit;
  signal fryx : std_logic;
  signal vu : bit;
  signal er : string(2 to 2);
  signal jxucs : real;
  signal rf : integer;
begin
  uwqhaqc : entity work.nqm
    port map (slspflyw => rf, bkgrx => jxucs, ynvlfgair => er, quuy => z);
  fnzpbgs : entity work.qa
    port map (tkecmam => vu);
  esdisq : entity work.biyzwwdvk
    port map (jrdyrfynyh => fryx);
  d : entity work.qa
    port map (tkecmam => iqmcqq);
  
  -- Single-driven assignments
  crnvpecuiv <= (others => '0');
  rf <= 14214;
  
  -- Multi-driven assignments
  fryx <= '0';
  fryx <= '0';
end wghsetws;



-- Seed after: 3814123237889790954,5805648483995786113
