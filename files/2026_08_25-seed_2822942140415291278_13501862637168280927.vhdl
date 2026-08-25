-- Seed: 2822942140415291278,13501862637168280927

entity rpvxrnib is
  port (oawmczm : linkage time; zdlzybwaq : out bit_vector(3 to 2));
end rpvxrnib;

architecture ypiv of rpvxrnib is
  
begin
  -- Single-driven assignments
  zdlzybwaq <= (others => '0');
end ypiv;

entity rqlmwoxa is
  port (jxxkkv : out time);
end rqlmwoxa;

architecture qqckypxogm of rqlmwoxa is
  signal dj : bit_vector(3 to 2);
  signal jukg : time;
  signal sjoliaj : bit_vector(3 to 2);
  signal bspnqwp : time;
  signal ah : bit_vector(3 to 2);
  signal zwjyfmkwsl : time;
begin
  lfyrdw : entity work.rpvxrnib
    port map (oawmczm => zwjyfmkwsl, zdlzybwaq => ah);
  z : entity work.rpvxrnib
    port map (oawmczm => bspnqwp, zdlzybwaq => sjoliaj);
  zejvjihpmy : entity work.rpvxrnib
    port map (oawmczm => jukg, zdlzybwaq => dj);
  
  -- Single-driven assignments
  jxxkkv <= zwjyfmkwsl;
end qqckypxogm;

entity wammrcq is
  port (jhhl : buffer real; tbnyimmu : in string(4 downto 1));
end wammrcq;

architecture xnpwssks of wammrcq is
  signal p : bit_vector(3 to 2);
  signal bvitkzuju : time;
  signal rbgbf : bit_vector(3 to 2);
  signal pokeueb : time;
begin
  mf : entity work.rpvxrnib
    port map (oawmczm => pokeueb, zdlzybwaq => rbgbf);
  tbd : entity work.rpvxrnib
    port map (oawmczm => bvitkzuju, zdlzybwaq => p);
end xnpwssks;

entity lcs is
  port (stdlzp : out real; byci : buffer integer);
end lcs;

architecture gzolqsls of lcs is
  signal pcze : bit_vector(3 to 2);
  signal hxrjwjcxa : time;
  signal tk : bit_vector(3 to 2);
  signal rz : time;
  signal sjpekyoohd : time;
begin
  wgfhizqn : entity work.rqlmwoxa
    port map (jxxkkv => sjpekyoohd);
  tmlefv : entity work.rpvxrnib
    port map (oawmczm => rz, zdlzybwaq => tk);
  walbxcyp : entity work.rpvxrnib
    port map (oawmczm => hxrjwjcxa, zdlzybwaq => pcze);
  
  -- Single-driven assignments
  byci <= 16#7_E_2#;
end gzolqsls;



-- Seed after: 2996522406826991596,13501862637168280927
