-- Seed: 11505905585486750391,6140041381800297705

entity xt is
  port (b : inout severity_level);
end xt;

architecture mvvacarjh of xt is
  
begin
  -- Single-driven assignments
  b <= FAILURE;
end mvvacarjh;

library ieee;
use ieee.std_logic_1164.all;

entity snpxixckcf is
  port (gtaqpjkrhu : out std_logic_vector(4 to 2));
end snpxixckcf;

architecture bgejkv of snpxixckcf is
  signal gbzzs : severity_level;
  signal jiae : severity_level;
  signal vorkl : severity_level;
begin
  lqfvmgg : entity work.xt
    port map (b => vorkl);
  kbv : entity work.xt
    port map (b => jiae);
  lgmg : entity work.xt
    port map (b => gbzzs);
end bgejkv;

entity jrjr is
  port (ea : buffer time; eu : in real);
end jrjr;

library ieee;
use ieee.std_logic_1164.all;

architecture vneeqwxl of jrjr is
  signal tiv : severity_level;
  signal uy : severity_level;
  signal qxfmrn : std_logic_vector(4 to 2);
  signal mem : severity_level;
begin
  pzbgkqle : entity work.xt
    port map (b => mem);
  bjnsvlh : entity work.snpxixckcf
    port map (gtaqpjkrhu => qxfmrn);
  bsvvadhu : entity work.xt
    port map (b => uy);
  huhbq : entity work.xt
    port map (b => tiv);
end vneeqwxl;

entity wsfmps is
  port (saclfnr : inout real);
end wsfmps;

library ieee;
use ieee.std_logic_1164.all;

architecture mbguluea of wsfmps is
  signal oyk : std_logic_vector(4 to 2);
  signal xdqgivq : time;
begin
  afkc : entity work.jrjr
    port map (ea => xdqgivq, eu => saclfnr);
  gdckdd : entity work.snpxixckcf
    port map (gtaqpjkrhu => oyk);
  p : entity work.snpxixckcf
    port map (gtaqpjkrhu => oyk);
  
  -- Single-driven assignments
  saclfnr <= saclfnr;
  
  -- Multi-driven assignments
  oyk <= oyk;
  oyk <= oyk;
end mbguluea;



-- Seed after: 4076346854798659881,6140041381800297705
