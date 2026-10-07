-- Seed: 13978028110415409423,5906004015519833893

entity zbc is
  port (myxp : buffer boolean_vector(0 to 2); odkfkuajg : out string(4 downto 5); ygk : linkage real);
end zbc;

architecture clpvxtpyv of zbc is
  
begin
  -- Single-driven assignments
  odkfkuajg <= "";
  myxp <= myxp;
end clpvxtpyv;

entity ikozhit is
  port (bnceyob : out time; nsxgdzgkdd : buffer time; fwf : out character);
end ikozhit;

architecture mplxtiv of ikozhit is
  signal xyiwcqvyhb : real;
  signal oiw : string(4 downto 5);
  signal syvmf : boolean_vector(0 to 2);
  signal crxnh : real;
  signal tft : string(4 downto 5);
  signal abutugehv : boolean_vector(0 to 2);
  signal ndnnw : real;
  signal feb : string(4 downto 5);
  signal libihcyak : boolean_vector(0 to 2);
begin
  g : entity work.zbc
    port map (myxp => libihcyak, odkfkuajg => feb, ygk => ndnnw);
  dmt : entity work.zbc
    port map (myxp => abutugehv, odkfkuajg => tft, ygk => crxnh);
  i : entity work.zbc
    port map (myxp => syvmf, odkfkuajg => oiw, ygk => xyiwcqvyhb);
  
  -- Single-driven assignments
  nsxgdzgkdd <= 4_2.4 us;
end mplxtiv;

library ieee;
use ieee.std_logic_1164.all;

entity pqpbsijak is
  port (mvjangwqls : linkage std_logic_vector(0 to 0));
end pqpbsijak;

architecture ub of pqpbsijak is
  signal ks : real;
  signal umq : string(4 downto 5);
  signal rv : boolean_vector(0 to 2);
  signal eekmc : character;
  signal mgbzcc : time;
  signal nbwldspd : time;
  signal ltswn : real;
  signal d : string(4 downto 5);
  signal qoihc : boolean_vector(0 to 2);
  signal mfcl : real;
  signal rlpk : string(4 downto 5);
  signal sljgbxqkg : boolean_vector(0 to 2);
begin
  ralvhnpdyz : entity work.zbc
    port map (myxp => sljgbxqkg, odkfkuajg => rlpk, ygk => mfcl);
  noibmh : entity work.zbc
    port map (myxp => qoihc, odkfkuajg => d, ygk => ltswn);
  mmzlhh : entity work.ikozhit
    port map (bnceyob => nbwldspd, nsxgdzgkdd => mgbzcc, fwf => eekmc);
  leayaqq : entity work.zbc
    port map (myxp => rv, odkfkuajg => umq, ygk => ks);
end ub;



-- Seed after: 6422754088440241698,5906004015519833893
