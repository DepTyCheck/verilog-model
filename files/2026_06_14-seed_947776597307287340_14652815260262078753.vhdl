-- Seed: 947776597307287340,14652815260262078753

entity uhvbfhzy is
  port (p : in time; snhfspnq : in time_vector(0 downto 3); hmxqsyep : buffer bit_vector(3 downto 0));
end uhvbfhzy;

architecture yd of uhvbfhzy is
  
begin
  -- Single-driven assignments
  hmxqsyep <= ('1', '1', '1', '0');
end yd;

entity zeieuhqh is
  port (mwljhau : inout character; xfrmankoc : inout real);
end zeieuhqh;

architecture dj of zeieuhqh is
  signal prnywyynxf : bit_vector(3 downto 0);
  signal axlyhdw : time_vector(0 downto 3);
  signal nsxcojk : time;
  signal dzdfconm : bit_vector(3 downto 0);
  signal cdt : time;
  signal odhzpe : bit_vector(3 downto 0);
  signal li : time_vector(0 downto 3);
  signal ayn : bit_vector(3 downto 0);
  signal cvjesh : time_vector(0 downto 3);
  signal igsglgwd : time;
begin
  nanfzk : entity work.uhvbfhzy
    port map (p => igsglgwd, snhfspnq => cvjesh, hmxqsyep => ayn);
  xyvnvt : entity work.uhvbfhzy
    port map (p => igsglgwd, snhfspnq => li, hmxqsyep => odhzpe);
  bca : entity work.uhvbfhzy
    port map (p => cdt, snhfspnq => cvjesh, hmxqsyep => dzdfconm);
  yivtvnfa : entity work.uhvbfhzy
    port map (p => nsxcojk, snhfspnq => axlyhdw, hmxqsyep => prnywyynxf);
  
  -- Single-driven assignments
  cdt <= 2 ns;
end dj;

library ieee;
use ieee.std_logic_1164.all;

entity lq is
  port (zwdwrcb : buffer bit; pinawoacul : out bit_vector(2 to 2); tkrcql : out std_logic_vector(3 to 0));
end lq;

architecture qwjexccxs of lq is
  signal di : bit_vector(3 downto 0);
  signal fmqno : time_vector(0 downto 3);
  signal gyi : time;
  signal inlpghub : real;
  signal ldokjbeay : character;
  signal xoy : bit_vector(3 downto 0);
  signal aj : time_vector(0 downto 3);
  signal qrddzrwhlt : time;
begin
  dmxebt : entity work.uhvbfhzy
    port map (p => qrddzrwhlt, snhfspnq => aj, hmxqsyep => xoy);
  quue : entity work.zeieuhqh
    port map (mwljhau => ldokjbeay, xfrmankoc => inlpghub);
  ciqjkkwonm : entity work.uhvbfhzy
    port map (p => gyi, snhfspnq => fmqno, hmxqsyep => di);
  
  -- Single-driven assignments
  pinawoacul <= (others => '0');
  
  -- Multi-driven assignments
  tkrcql <= (others => '0');
  tkrcql <= "";
  tkrcql <= (others => '0');
  tkrcql <= (others => '0');
end qwjexccxs;



-- Seed after: 12968412319232480884,14652815260262078753
