-- Seed: 10566677503621043579,6299883410057943775

entity pdxf is
  port (bkcu : in boolean_vector(3 downto 0); xozqpryjg : in bit; f : inout severity_level);
end pdxf;

architecture riwcurn of pdxf is
  
begin
  -- Single-driven assignments
  f <= WARNING;
end riwcurn;

entity qfhzb is
  port (nb : inout real);
end qfhzb;

architecture thdgqldjcu of qfhzb is
  signal rqinpaoo : severity_level;
  signal etwjxpot : severity_level;
  signal vuyoepvwee : bit;
  signal rjfiynrs : severity_level;
  signal hwnup : bit;
  signal zvb : boolean_vector(3 downto 0);
begin
  alc : entity work.pdxf
    port map (bkcu => zvb, xozqpryjg => hwnup, f => rjfiynrs);
  xrx : entity work.pdxf
    port map (bkcu => zvb, xozqpryjg => vuyoepvwee, f => etwjxpot);
  z : entity work.pdxf
    port map (bkcu => zvb, xozqpryjg => hwnup, f => rqinpaoo);
  
  -- Single-driven assignments
  hwnup <= '0';
  vuyoepvwee <= hwnup;
  zvb <= zvb;
  nb <= nb;
end thdgqldjcu;

entity rgrd is
  port (bnjfws : inout time);
end rgrd;

architecture nlkwwpvh of rgrd is
  
begin
  
end nlkwwpvh;



-- Seed after: 9228525149905581376,6299883410057943775
