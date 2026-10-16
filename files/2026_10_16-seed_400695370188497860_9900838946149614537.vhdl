-- Seed: 400695370188497860,9900838946149614537

entity n is
  port (bwnn : out time);
end n;

architecture rdbr of n is
  
begin
  -- Single-driven assignments
  bwnn <= 2#1# ps;
end rdbr;

entity wcpfsifc is
  port (yydyielpr : in time);
end wcpfsifc;

architecture slpp of wcpfsifc is
  signal kpn : time;
  signal vx : time;
  signal eeljtyt : time;
  signal dmczsbb : time;
begin
  hrgksdu : entity work.n
    port map (bwnn => dmczsbb);
  g : entity work.n
    port map (bwnn => eeljtyt);
  gsjxeeg : entity work.n
    port map (bwnn => vx);
  vlc : entity work.n
    port map (bwnn => kpn);
end slpp;



-- Seed after: 15429870831864862362,9900838946149614537
