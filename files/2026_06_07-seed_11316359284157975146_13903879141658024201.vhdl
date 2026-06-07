-- Seed: 11316359284157975146,13903879141658024201



entity dnjbda is
  port (kfxrwc : buffer integer_vector(1 to 3); svsfilexlh : linkage integer_vector(2 to 1); umf : inout time);
end dnjbda;



architecture yi of dnjbda is
  
begin
  
end yi;

library ieee;
use ieee.std_logic_1164.all;

entity hbvladkbn is
  port (jfoa : inout severity_level; uteijlqp : buffer std_logic; bntxrn : inout integer; znlm : inout time);
end hbvladkbn;



architecture vcmuz of hbvladkbn is
  signal kuajdlmrj : integer_vector(1 to 3);
  signal eet : time;
  signal vcqkxn : integer_vector(2 to 1);
  signal j : integer_vector(1 to 3);
begin
  jixvvhlqux : entity work.dnjbda
    port map (kfxrwc => j, svsfilexlh => vcqkxn, umf => eet);
  wfrnlp : entity work.dnjbda
    port map (kfxrwc => kuajdlmrj, svsfilexlh => vcqkxn, umf => znlm);
end vcmuz;



entity q is
  port (ta : buffer boolean; iuhz : buffer real; ezwo : buffer real);
end q;



architecture xdcmiww of q is
  signal bbtpcd : time;
  signal ejgrvjj : integer_vector(1 to 3);
  signal mftj : time;
  signal xuwkqwgsq : integer_vector(2 to 1);
  signal ltgvvfhv : integer_vector(1 to 3);
  signal nkjesaw : time;
  signal qs : integer_vector(2 to 1);
  signal shlueaqtzk : integer_vector(1 to 3);
begin
  mdpnio : entity work.dnjbda
    port map (kfxrwc => shlueaqtzk, svsfilexlh => qs, umf => nkjesaw);
  nstyjjqi : entity work.dnjbda
    port map (kfxrwc => ltgvvfhv, svsfilexlh => xuwkqwgsq, umf => mftj);
  kke : entity work.dnjbda
    port map (kfxrwc => ejgrvjj, svsfilexlh => qs, umf => bbtpcd);
end xdcmiww;



-- Seed after: 1893407800035845388,13903879141658024201
