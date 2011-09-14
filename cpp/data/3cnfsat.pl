#!/usr/bin/env perl

use warnings;
use strict;

package Node;

my @allNodes = ();

sub new {
  my ($class, $name) = @_;

  my $self = { name => $name,
	       neighbors => [ ],
	     };
  bless($self, $class);

  push @allNodes, $self;

  return $self;
}

sub name {
  my ($self) = @_;
  return $self->{name};
}

sub addEdge {
  my ($self, $other) = @_;

  push @{$self->{neighbors}}, $other;
  push @{$other->{neighbors}}, $self;
}

sub neighbors {
  my ($self) = @_;

  return $self->{neighbors};
}

sub dumpNodes {

  print scalar @allNodes, "\n";

  foreach my $node (@allNodes) {
    print $node->name(), ":", join(",", map { $_->name() } @{$node->neighbors()}), "\n";
  }
}



#print it

package main;

sub toAlphaBase {
    my ($num) = @_;

    my $result = "";

    while ($num > 0) {
      $result = chr(($num % 26) + ord('A')) . $result;
      $num = int($num / 26);
	
    }
    return $result;
}

sub assembleGadget {
  my ($clauseNum, $true, $false, $term1, $term2, $term3) = @_;

  my $clause = toAlphaBase($clauseNum);

  # Six nodes
  my $nodeA1 = Node->new("${clause}AA");
  my $nodeA2 = Node->new("${clause}AB");
  my $nodeA3 = Node->new("${clause}AC");

  my $nodeB1 = Node->new("${clause}BA");
  my $nodeB2 = Node->new("${clause}BB");
  my $nodeB3 = Node->new("${clause}BC");

  # Thirteen edges
  $nodeA1->addEdge($nodeB1);
  $nodeA2->addEdge($nodeB2);
  $nodeA3->addEdge($nodeB3);

  $nodeA1->addEdge($term1);
  $nodeA2->addEdge($term2);
  $nodeA3->addEdge($term3);

  $nodeA1->addEdge($true);
  $nodeA2->addEdge($true);
  $nodeA3->addEdge($true);

  $nodeB1->addEdge($true);
  $nodeB1->addEdge($nodeB2);
  $nodeB2->addEdge($nodeB3);
  $nodeB3->addEdge($false);
}

sub main {
  my @clauses;
  my %termPairs;

  my $true = Node->new("TRUE");
  my $false = Node->new("FALSE");
  my $meta = Node->new("META");

  # The triangle
  $true->addEdge($false);
  $true->addEdge($meta);
  $false->addEdge($meta);

  my $clauseNum = 0;
  while (scalar @ARGV) {
    my @clauseTerms;
    for (my $i = 0; $i < 3; $i++) {
      die "Arguments must be in sets of three" unless scalar @ARGV;

      my $arg = shift @ARGV;

      my ($not, $name) = ($arg =~ m/^(-)?(.)$/);

      die "Arguments must be one char terms, optionally prefixed by '-'" unless defined $name;

      my $terms = $termPairs{$name};
      unless (defined $terms) {
	my $term = Node->new($name);
	my $notTerm = Node->new("NOT${name}");

	# All terms and their negation connected to META
	$term->addEdge($meta);
	$notTerm->addEdge($meta);

	# Each term connected to its negation
	$term->addEdge($notTerm);

	$terms = [ $term, $notTerm ];
	$termPairs{$name} = $terms;
      }
      my $term;
      if (defined $not) {
	$term = $terms->[1];
      } else {
	$term = $terms->[0];
      }
      push @clauseTerms, $term;
    }

    assembleGadget($clauseNum, $true, $false, @clauseTerms);

    $clauseNum++;
  }

  Node->dumpNodes();
}

if (! scalar @ARGV) {
  print <<EOF;
Arguments must occur in sets of three.  Arguments must be one-character
terms, optionally preceded by a "-" for a negated term.
EOF
  exit(0);
}

main();
