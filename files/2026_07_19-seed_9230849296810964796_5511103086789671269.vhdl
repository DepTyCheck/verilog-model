-- Seed: 9230849296810964796,5511103086789671269

entity gwhdiq is
  port (tuaoth : inout integer);
end gwhdiq;

architecture c of gwhdiq is
  
begin
  -- Single-driven assignments
  tuaoth <= 33020;
end c;

entity bvbuwb is
  port (fnwihhfzqc : out severity_level; uk : buffer time; qdiztsb : in integer; tvkmc : buffer bit);
end bvbuwb;

architecture wbmjjpovfq of bvbuwb is
  signal o : integer;
  signal qnnlkyeedh : integer;
  signal enjrt : integer;
  signal bftxvombae : integer;
begin
  popi : entity work.gwhdiq
    port map (tuaoth => bftxvombae);
  exaondybmw : entity work.gwhdiq
    port map (tuaoth => enjrt);
  mtaofhp : entity work.gwhdiq
    port map (tuaoth => qnnlkyeedh);
  khmyy : entity work.gwhdiq
    port map (tuaoth => o);
  
  -- Single-driven assignments
  tvkmc <= '0';
  fnwihhfzqc <= fnwihhfzqc;
  uk <= 1 hr;
end wbmjjpovfq;

entity h is
  port (ias : out time);
end h;

architecture jpwzoopaq of h is
  signal xx : integer;
begin
  hbretxbnv : entity work.gwhdiq
    port map (tuaoth => xx);
  
  -- Single-driven assignments
  ias <= 8#105.2_6# ms;
end jpwzoopaq;

library ieee;
use ieee.std_logic_1164.all;

entity revusqsvq is
  port (hin : inout std_logic; qc : linkage boolean);
end revusqsvq;

architecture brlayea of revusqsvq is
  signal th : bit;
  signal patcbbpu : time;
  signal ofe : severity_level;
  signal xrtqs : time;
  signal iijjtpxa : integer;
  signal qakqpy : integer;
begin
  nohl : entity work.gwhdiq
    port map (tuaoth => qakqpy);
  fsbmq : entity work.gwhdiq
    port map (tuaoth => iijjtpxa);
  zxchh : entity work.h
    port map (ias => xrtqs);
  mxwsff : entity work.bvbuwb
    port map (fnwihhfzqc => ofe, uk => patcbbpu, qdiztsb => iijjtpxa, tvkmc => th);
  
  -- Multi-driven assignments
  hin <= hin;
end brlayea;



-- Seed after: 1527190819607833053,5511103086789671269
